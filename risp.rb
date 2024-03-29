#!/usr/bin/env ruby

require "bundler/setup"
require "hamster/hash"
require "readline"
require "readline/history/restore"
require "optimist"
require "pry-byebug"

Readline::History::Restore.new(File.expand_path("~/.risp_history"))

module Risp
  @options = Optimist::options do
    banner <<EOS
Usage: #{$0} [options] [file...]
Run the risp repl to interpret my tiny lisp dialect with lazy evaluation.
EOS
    opt :lazy, "Use lazy evaluation (default)"
    opt :strict, "Use strict evaluation"
    opt :macros, "Disable macros"
    opt :repl, "Run a repl after loading the files"
    opt :trace, "Print evaluation trace"
    opt :debug, "Debug with binding.pry on exception"
    conflicts :lazy, :strict
  end

  def self.lazy?
    !@options.strict
  end

  def self.macros?
    !@options.macros
  end

  @tracing = @options.trace
  def self.trace?
    @tracing
  end

  def self.debug?
    @options.debug
  end

  at_exit do
    # Currently this has to be true because printing causes the
    # expression's thunks to be evaluated.
    (["prelude.risp"] + ARGV).each do |file|
      execute(file, true)
    end
    if @options.repl || ARGV.length == 0
      ::Lepr.repl
    end
  end

  def self.execute(file, do_print)
    file = File.read(file)
    no_comments = file.gsub(/;.*/, "")
    no_comments.split(/\n{2,}/).each do |section|
      if section !~ /^\s*$/
        form = ::Lepr.parse(section)
        expanded = macros? ? do_macros(form) : form
        result = eval(expanded)
        if do_print
          result.write(STDOUT)
          puts
        end
      end
    end
  end

  @indent = ""
  def self.trace(string_proc, &block)
    if trace?
      indent = @indent
      STDERR.puts "#{indent}#{string_proc.call} {"
      @indent += "  "
      begin
        block.call.tap do |result|
          STDERR.puts "#{indent}} => #{result.inspect}"
        end
      ensure
        @indent = indent
      end
    else
      block.call
    end
  end

  module Atom
    def self.new(string)
      case string
      when /^[0-9]+$/
        Risp::Number.new(string)
      else
        Risp::Symbol.intern(string)
      end
    end

    def eval(bindings)
      self
    end

    def inspect
      to_s
    end

    def write(io, dethunk = true)
      io.write(to_s)
    end
  end

  class Symbol
    include Atom

    @symbols = Hash.new do |hash, name|
      hash[name] = Symbol.new(name)
    end

    def self.intern(name)
      @symbols[name]
    end

    def initialize(name)
      @name = name
    end

    def name
      @name
    end

    def get_binding(bindings)
      bindings.get(self) || Risp.global_bindings.get(self)
    end

    def eval(bindings)
      Risp.trace(->{"#{name}"}) do
        get_binding(bindings).tap do |result|
          if !result
            binding.pry if Risp.debug?
            raise Risp::Exception.new("No binding for #{self.inspect}")
          end
        end
      end
    end

    def to_s
      name
    end
  end

  class Number
    include Atom

    def initialize(thing)
      @val =
        case thing
        when Numeric
          thing
        when String
          thing.to_i
        else
          binding.pry if Risp.debug?
          raise "Bad arg to Number.new: #{thing.inspect}"
        end
    end

    def val
      @val
    end

    def +(other)
      if other.is_a?(Number)
        Number.new(self.val + other.val)
      else
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Can't apply \"+\" to #{other.inspect}")
      end
    end

    def -(other)
      if other.is_a?(Number)
        Number.new(self.val - other.val)
      else
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Can't apply \"-\" to #{other.inspect}")
      end
    end

    def *(other)
      if other.is_a?(Number)
        Number.new(self.val * other.val)
      else
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Can't apply \"*\" to #{other.inspect}")
      end
    end

    def ==(other)
      other.is_a?(Number) && self.val == other.val
    end

    def to_s
      val.to_s
    end
  end

  class Cell
    def initialize(car, cdr)
      @car = car
      @cdr = cdr
    end

    def car
      @car
    end

    def cdr
      @cdr
    end

    def inspect
      result = "("

      nxt = self
      while nxt != Qnil
        result << nxt.car.inspect
        nxt = nxt.cdr
        case nxt
        when Qnil
        when Cell
          result << " "
        else
          result << " . "
          result << nxt.inspect
          nxt = Qnil
        end
      end
      
      result << ")"
    end

    def write(io, dethunk = true)
      maybe_dethunk =
        if dethunk
          ->(val){Risp.dethunk(val)}
        else
          ->(val){val}
        end

      io.write("(")

      nxt = self
      while nxt != Qnil
        maybe_dethunk[Risp.car(nxt)].write(io, dethunk)
        nxt = maybe_dethunk[Risp.cdr(nxt)]
        case nxt
        when Qnil
        when Cell
          io.write(" ")
        else
          io.write(" . ")
          nxt.write(io, dethunk)
          nxt = Qnil
        end
      end

      io.write(")")
    end
  end

  module HasArgList
    def check_arg_list(symbols)
      case symbols
      when Qnil
        # ok
      when Cell
        a = Risp.car(symbols)
        d = Risp.cdr(symbols)
        case a
        when Qrest
          if d.is_a?(Cell) &&
             Risp.car(d).is_a?(Symbol) && Risp.car(d) != Qnil &&
             Risp.cdr(d) == Qnil
            # ok
          else
            raise Risp::Exception.new("Exactly one symbol required after &rest, got #{d.inspect}")
          end
        when Symbol
          check_arg_list(d)
        else
          raise Risp::Exception.new("Expected symbol, got #{a.inspect}")
        end
      else
        raise Risp::Exception.new("Expected symbol list, got #{symbols.inspect}")
      end
    end

    def get_arity(symbols)
      array = Risp::to_array(symbols)
      if array.include?(Qrest)
        Qnil
      else
        Number.new(array.length)
      end
    end

    def bind_symbols_to_args(bindings, symbols, args)
      case
      when symbols == Qnil && args == Qnil
        bindings
      when symbols == Qnil
        binding.pry if ::Risp.debug?
        raise ::Risp::Exception.new("Too many arguments to #{@name}")
      when ::Risp.car(symbols) == Qrest
        bindings.bind(::Risp.car(::Risp.cdr(symbols)), args)
      when args == Qnil
        binding.pry if ::Risp.debug?
        raise ::Risp::Exception.new("Not enough arguments for #{@name}")
      else
        bind_symbols_to_args(
          bindings.bind(::Risp.car(symbols), ::Risp.car(args)),
          ::Risp.cdr(symbols), ::Risp.cdr(args))
      end
    end
  end

  class Closure
    include Atom
    include HasArgList

    def initialize(symbols, form, bindings, name)
      symbols = Risp.dethunk_deep(symbols)
      form = Risp.dethunk_deep(form)
      check_arg_list(symbols)
      @symbols = symbols
      @form = form
      @bindings = bindings
      @name = name || form.inspect
      @arity = get_arity(@symbols)
    end

    def arity
      @arity
    end

    def apply(args, _bindings = nil)
      Risp.trace(->{"#{@name}#{args.inspect}"}) do
        bindings = bind_symbols_to_args(@bindings, @symbols, args)
        Risp.eval(@form, bindings)
      end
    rescue Risp::Exception
      raise Risp::Exception.new("in #{@name}#{args.inspect}")
    end

    def inspect
      io = StringIO.new
      write(io, false)
      io.string
    end

    def write(io, dethunk = true)
      io.write("[#{@symbols.inspect} => ")
      @form.write(io, dethunk)
      io.write(" ")
      io.write(@bindings.inspect)
      io.write("]")
    end
  end

  class Macro
    include Atom
    include HasArgList

    def initialize(symbols, form, name)
      symbols = Risp.dethunk_deep(symbols)
      form = Risp.dethunk_deep(form)
      check_arg_list(symbols)
      @symbols = symbols
      @form = form
      @name = name || form.inspect
    end

    def apply(args, bindings)
      Risp.trace(->{"#{@name}#{args.inspect}"}) do
        # We are using the caller's bindings, this is so quasiquote, which
        # is a macro, has access to the caller's bindings.  Probably
        # not the right way to do it, but oh well.
        new_bindings = bind_symbols_to_args(bindings, @symbols, args)
        Risp.eval_strict(@form, new_bindings)
      end
    end

    def inspect
      io = StringIO.new
      write(io, false)
      io.string
    end

    def write(io, dethunk = true)
      io.write("[macro #{@symbols.inspect} => ")
      @form.write(io, dethunk)
      io.write("]")
    end
  end

  class Thunk
    def initialize(form, bindings)
      @state = :unevaluated
      @form = form
      @bindings = bindings
    end

    def eval
      case @state
      when :unevaluated
        @state = :in_progress
        Risp.dethunk(Risp.eval_strict(@form, @bindings)).tap do |result|
          @memo = result
          @state = :evaluated
          # Clear @form and @bindings so any thunks in the bindings
          # can be garbage collected.
          @form = nil
          @bindings = nil
        end
      when :in_progress
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Infinite loop")
      when :evaluated
        @memo
      end
    rescue Risp::Exception
      raise Risp::Exception.new("in thunk #{@form.inspect}")
    end

    def inspect
      if !@inspecting
        @inspecting = true
        begin
          "[thunk: <#{@memo && @memo.inspect}> #{@form.inspect}, #{@bindings.inspect}]"
        ensure
          @inspecting = false
        end
      else
        "[thunk: <...> #{@form.inspect}, #{@bindings.inspect}]"
      end
    end

    def write(io, dethunk = true)
      # XXX if we don't dethunk we can get a long chain of thunks returning
      # thunks and making deeply nested calls to write.  But is this is an
      # ok place to dethunk?  Should eval do the dethunk itself?
      # See commit 8fb6c578187beb89baeb0865a26845b81e98bd7e.
      Risp.dethunk(eval).write(io, dethunk)
    end
  end

  class Bindings
    def initialize(hash = Hamster::Hash.new)
      @hash = hash
    end

    def bind(symbol, val = Slot.new)
      Bindings.new(@hash.put(symbol, val))
    end

    def set(symbol, val)
      @hash[symbol].val = val
    end

    def get(symbol)
      val = @hash[symbol]
      case val
      when Slot
        val.val
      else
        val
      end
    end

    # Define [] on strings for easy debugging.

    def [](string)
      get(Risp::Symbol.intern(string))
    end

    def inspect
      # letrec creates bindings that contain themselves.  Here we
      # detect cycles and just return "{...}".
      if !@inspecting
        @inspecting = true
        begin
          vals = @hash.flatten(0).map do |k, v|
            "#{k.inspect}: #{v.inspect}"
          end
          "{" + vals.join(", ") + "}"
        ensure
          @inspecting = false
        end
      else
        "{...}"
      end
    end

    def write(io, dethunk = true)
      io.write(inspect)
    end

    # This is for letrec where we have to set the value after the
    # binding is created.
    class Slot
      attr_accessor :val
    end
  end

  module Builtin
    include Atom

    def initialize(name, nargs, &block)
      @name = name
      @nargs = nargs
      @block = block
      @arity = @nargs ? Number.new(@nargs) : Qnil
    end

    def arity
      @arity
    end

    def apply(args, bindings)
      Risp.trace(->{"#{@name}#{args.inspect}"}) do
        if @nargs
          array = Risp::to_array(args, @nargs)
          @block.call(*array, bindings)
        else
          @block.call(args, bindings)
        end
      end
    rescue Risp::Exception
      raise Risp::Exception.new("in #{self.inspect}#{args.inspect}")
    end

    def inspect
      "<#{self.class}: #{@name}>"
    end

    def write(io, dethunk = true)
      io.write(inspect)
    end
  end

  class Subr
    include Builtin
  end

  class Fsubr
    include Builtin
  end

  @default_bindings = Bindings.new

  @global_bindings = @default_bindings

  def self.global_bindings
    @global_bindings
  end

  def self.global(symbol, val)
    @global_bindings = @global_bindings.bind(symbol, val)
  end

  Qnil = Symbol.intern("nil")

  # Make Qnil show up as "Qnil" in the debugger to differntiate it
  # from ruby's nil.

  def Qnil.inspect
    "Qnil"
  end

  Qt = Symbol.intern("t")

  Qrest = Symbol.intern("&rest")

  Qquote = Symbol.intern("quote")
  Qquasiquote = Symbol.intern("quasiquote")
  Qunquote = Symbol.intern("unquote")
  Qunquote_splicing = Symbol.intern("unquote-splicing")
  Qdo_macros = Symbol.intern("do-macros")
  Qinspect = Symbol.intern("inspect")
  Qpry = Symbol.intern("pry")

  global(Symbol.intern("lazy?"), lazy? ? Qt : Qnil)

  def self.fsubr(name, nargs = nil, f_name = name, &block)
    Fsubr.new(name, nargs, &block).tap do |subr|
      global(Symbol.intern(name), subr)
      define_singleton_method(f_name.to_sym, &block)
    end
  end

  def self.subr(name, nargs = nil, f_name = name, &block)
    Subr.new(name, nargs, &block).tap do |subr|
      global(Symbol.intern(name), subr)
      define_singleton_method(f_name.to_sym, &block)
    end
  end

  fsubr("quote", 1) do |arg, bindings = nil|
    arg
  end
  
  subr("eq", 2) do |x, y, bindings = nil|
    to_boolean(dethunk(x) == dethunk(y))
  end

  subr("car", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    if arg.is_a?(Cell)
      arg.car
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Bad arg to car: #{arg.inspect}")
    end
  end

  subr("cdr", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    if arg.is_a?(Cell)
      arg.cdr
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Bad arg to cdr: #{arg.inspect}")
    end
  end

  subr("cons", 2) do |x, y, bindings = nil|
    Cell.new(x, y)
  end

  fsubr("cond") do |form, bindings|
    case
    when form == Qnil
      Qnil
    when dethunk(eval_strict(car(car(form)), bindings)) != Qnil
      eval(car(cdr(car(form))), bindings)
    else
      cond(cdr(form), bindings)
    end
  end

  fsubr("lambda", 2) do |symbols, form, bindings, name = nil|
    Closure.new(symbols, form, bindings, name)
  end

  subr("null?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg == Qnil)
  end

  subr("atom?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg.is_a?(Atom))
  end

  subr("symbol?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg.is_a?(Symbol))
  end

  subr("bound?", 1) do |arg, bindings|
    arg = dethunk(arg)
    case arg
    when Symbol
      to_boolean(arg.get_binding(bindings))
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Bad arg to bound?: #{arg.inspect}")
    end
  end

  subr("cons?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg.is_a?(Cell))
  end

  subr("list?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg == Qnil || arg.is_a?(Cell))
  end

  subr("macro?", 1) do |arg, bindings = nil|
    arg = dethunk(arg)
    to_boolean(arg.is_a?(Macro))
  end

  @subr_eval = subr("eval", 1) do |expr, bindings = @default_bindings|
    begin
      case expr
      when Atom
        # Don't bother to make thunks just to look up atom bindings.
        # Assume the global_bindings won't change while we're evaluating
        # an expression. Numbers certainly never change.  Neither should
        # Qt and Qnil.
        expr.eval(bindings)
      else
        if lazy?
          Thunk.new(expr, bindings)
        else
          eval_strict(expr, bindings)
        end
      end
    rescue Risp::Exception
      raise Risp::Exception.new("in #{expr.inspect}")
    end
  end

  @subr_eval_strict = Subr.new("#eval_strict", 1) do |expr, bindings|
    eval_strict(expr, bindings)
  end

  def self.eval_strict(expr, bindings = @default_bindings)
    expr = dethunk(expr)
    case expr
    when Atom
      expr.eval(bindings)
    when Cell
      fn = eval_strict(car(expr), bindings)
      args = cdr(expr)
      # This method is eval_strict: the actual result is needed by the caller,
      # so use eval_strict instead eval.  This fixes the problem where
      # (eval '(+ 1 3)) returns (+ 1 3) instead of 4.
      #
      if fn == @subr_eval
        fn = @subr_eval_strict
      end
      apply(fn, args, bindings)
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Don't know how to eval #{expr.inspect}")
    end
  end

  def self.dethunk(arg)
    if arg.is_a?(Thunk)
      arg.eval
    else
      arg
    end
  end

  def self.dethunk_deep(arg)
    arg = Risp.dethunk(arg)
    case arg
    when Cell
      Cell.new(dethunk_deep(arg.car), dethunk_deep(arg.cdr))
    else
      arg
    end
  end

  subr_apply = subr("apply", 2) do |fn, args, bindings|
    fn = dethunk(fn)
    args = dethunk(args)
    case fn
    when subr_apply
      # This is really nasty but without this "when" an applied subr
      # gets its arguments evaluated an extra time.
      evalled_args = eval_list(args, bindings)
      apply_fn = dethunk(car(evalled_args))
      apply_args = car(cdr(evalled_args))
      apply_fn.apply(apply_args, bindings)
    when Fsubr
      fn.apply(args, bindings)
    when Subr
      fn.apply(eval_list(args, bindings), bindings)
    when Closure
      fn.apply(eval_list(args, bindings))
    when Macro
      fn.apply(args, bindings)
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Don't know how to apply #{fn.inspect}")
    end
  end

  subr("not", 1) do |arg, bindings = nil|
    null?(arg)
  end

  # This can't use fold_left because it needs to short-circuit.
  #
  fsubr("and", nil, :f_and) do |args, bindings|
    trampoline do
      _and(args, bindings)
    end
  end

  def self._and(args, bindings)
    args = dethunk(args)
    case
    when args == Qnil
      Qt
    when args.is_a?(Cell)
      val = dethunk(eval_strict(args.car, bindings))
      case
      when val == Qnil
        Qnil
      when dethunk(args.cdr) == Qnil
        val
      else
        -> do
          _and(args.cdr, bindings)
        end
      end
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't do \"and\" with #{args.inspecct}")
    end
  end

  # This can't use fold_left because it needs to short-circuit.
  #
  fsubr("or", nil, :f_or) do |args, bindings|
    trampoline do
      _or(args, bindings)
    end
  end

  def self._or(args, bindings)
    args = dethunk(args)
    case
    when args == Qnil
      Qnil
    when args.is_a?(Cell)
      val = dethunk(eval_strict(args.car, bindings))
      if val != Qnil
        val
      else
        -> do
          _or(args.cdr, bindings)
        end
      end
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't apply \"and\" to #{args.inspect}")
    end
  end

  fsubr("+") do |list, bindings|
    fold_left(Number.new(0), list) do |memo, arg|
      memo + dethunk(eval_strict(arg, bindings))
    end
  end

  # This is trickier than "+" because if there is one argument then it
  # is negated, otherwise all the other arguments are subtracted from
  # the first.
  #
  fsubr("-") do |list, bindings|
    list = dethunk(list)
    case list
    when Cell
      val = dethunk(eval_strict(list.car, bindings))
      case val
      when Number
        if dethunk(list.cdr) == Qnil
          Number.new(-val.val)
        else
          fold_left(val, list.cdr) do |memo, arg|
            memo - dethunk(eval_strict(arg, bindings))
          end
        end
      else
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Can't apply \"-\" to #{val.inspect}")
      end
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't apply \"-\" to #{list.inspect}")
    end
  end

  fsubr("*") do |list, bindings|
    fold_left(Number.new(1), list) do |memo, arg|
      memo * dethunk(eval_strict(arg, bindings))
    end
  end

  subr("list", nil) do |list, bindings = nil|
    dethunk(list)
  end

  fsubr("let", 2) do |bind_list, form, bindings|
    bind_array = Risp::to_array(bind_list)

    check_symbols(
      bind_array.map do |e|
        symbol, expr = to_array(e, 2)
        symbol
      end)

    new_bindings = bind_array.reduce(bindings) do |memo, e|
      symbol, expr = to_array(e, 2)
      val = eval(expr, bindings)
      memo.bind(symbol, val)
    end

    eval(form, new_bindings)
  end

  fsubr("let*", 2) do |bind_list, form, bindings|
    bind_array = Risp::to_array(bind_list)

    check_symbols(
      bind_array.map do |e|
        symbol, expr = to_array(e, 2)
        symbol
      end)

    new_bindings = bind_array.reduce(bindings) do |memo, e|
      symbol, expr = to_array(e, 2)
      val = eval(expr, memo)
      memo.bind(symbol, val)
    end

    eval(form, new_bindings)
  end

  fsubr("letrec", 2) do |bind_list, form, bindings|
    bind_array = Risp::to_array(bind_list)

    check_symbols(
      bind_array.map do |e|
        symbol, expr = to_array(e, 2)
        symbol
      end)

    # Create the new bindings with the names initially unbound.  This sucks.
    # We need to do this because we can't bake the bindings into Closures
    # until we've added the names to them, but we can't bind the values
    # until we have the Closures.

    new_bindings = bind_array.reduce(bindings) do |memo, e|
      symbol, expr = to_array(e, 2)
      memo.bind(symbol)
    end

    # Now bind to the names.  each is a non-functional side-effect thing.

    bind_array.each do |e|
      symbol, expr = to_array(e, 2)
      # Now we can bake the bindings into any closures.
      val = eval(expr, new_bindings)
      # And finally bind the symbol to its closure.
      new_bindings.set(symbol, val)
    end
      
    # Finally evaluate the form with the letrec bindings.

    eval(form, new_bindings)
  end

  fsubr("define", 2) do |args, form, bindings|
    args = dethunk(args)
    case args
    when Symbol
      eval(form, bindings).tap do |val|
        global(args, val)
      end
    when Cell
      name = dethunk(args.car)
      symbols = args.cdr
      lambda(symbols, form, bindings, name.to_s).tap do |closure|
        global(name, closure)
      end
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't define #{args.inspect}")
    end
  end

  fsubr("define-macro", 2) do |args, form, bindings = nil|
    args = dethunk(args)
    case args
    when Cell
      name = dethunk(args.car)
      symbols = args.cdr
      Macro.new(symbols, form, name.to_s).tap do |macro|
        global(name, macro)
      end
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't define-macro #{args.inspect}")
    end
  end

  subr("gensym", 0) do |bindings = nil|
    @gensym_count ||= 0
    @gensym_count += 1
    # Note gensyms are not interned, so a gensym will be different
    # from anoy other Symbol even if they have the same name.
    Symbol.new("##{@gensym_count}")
  end

  subr("arity", 1) do |fn, bindings = nil|
    fn = dethunk(fn)
    if fn.respond_to?(:arity)
      fn.arity
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't get arity of #{fn.inspect}")
    end
  end

  fsubr("lazy", 1) do |form, bindings|
    Thunk.new(form, bindings)
  end

  subr("load", 1) do |symbol, bindings = nil|
    symbol = dethunk(symbol)
    execute("#{symbol.name}.risp", true)
    Qt
  end

  subr("pry", 1) do |arg, bindings = nil|
    binding.pry
    arg
  end

  subr("inspect", 1) do |arg, bindings = nil|
    puts arg.inspect
    arg
  end

  subr("trace-on", 0) do |bindings = nil|
    @tracing = true
    Qt
  end

  subr("xxx", 0) do |bindings = nil|
    @xxx = true
    Qt
  end

  def self.xxx
    @xxx
  end

  def self.do_macros(form)
    if bound?(Qdo_macros, @default_bindings) == Qt
      dethunk_deep(
        apply(eval(Qdo_macros, @default_bindings),
              to_list(to_list(Qquote, form)), @default_bindings))
    else
      form
    end
  end

  def self.to_boolean(arg)
    arg ? Qt : Qnil
  end

  def self.to_list(*elements)
    elements.reverse.reduce(Risp::Qnil) do |memo, element|
      cons(element, memo)
    end
  end

  def self.to_array(list, length = nil)
    fold_block([], list) do |memo, element|
      memo << element
    end.tap do |array|
      if length && array.length != length
        binding.pry if Risp.debug?
        raise Risp::Exception.new("Expected #{length} arguments, got #{array.length}")
      end
    end
  end

  def self.fold_block(memo, list, &block)
    list = dethunk(list)
    case list
    when Qnil
      memo
    when Cell
      new_memo = block.call(memo, list.car)
      fold_block(new_memo, list.cdr, &block)
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't fold #{list.inspect}")
    end
  end

  # fold_left can fold arbitrarily long lazy lists.

  def self.fold_left(accum, list, &block)
    while (list = dethunk(list)).is_a?(Cell)
      accum = block.call(accum, list.car)
      list = list.cdr
    end
    if list == Qnil
      accum
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't fold #{list.inspect}")
    end
  end

  def self.map_block(list, &block)
    list = dethunk(list)
    case list
    when Qnil
      Qnil
    when Cell
      cons(block.call(list.car), map_block(list.cdr, &block))
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Can't map #{list.inspect}")
    end
  end

  def self.eval_list(list, bindings)
    map_block(list) do |element|
      eval(element, bindings)
    end
  end

  def self.reverse(list)
    fold_block(Qnil, list) do |memo, element|
      cons(element, memo)
    end
  end

  def self.check_symbols(array)
    nonsymbols = array.select{|x| !x.is_a?(Symbol)}
    case nonsymbols.length
    when 0
      # Ok
    when 1
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Not a symbol: #{nonsymbols.first.inspect}")
    else
      binding.pry if Risp.debug?
      raise Risp::Exception.new("Not symbols: #{nonsymbols.map(&:inspect).join(" ")}")
    end
  end

  def self.trampoline(&block)
    loop do
      case block
      when Proc
        block = block.call
      else
        break block
      end
    end
  end

  class Exception < StandardError
    def initialize(message)
      @message = message
    end

    def to_s
      @message
    end
  end
end

class Lepr
  def self.repl
    while line = Readline.readline('> ', true)
      begin
        form = parse(line)
        expanded = Risp.macros? ? Risp::do_macros(form) : form
        Risp.eval_strict(expanded).tap do |val|
          val.write(STDOUT)
          puts
        end
      rescue Risp::Exception => ex
        show_original_cause = lambda do |ex|
          if !ex.cause || !(ex.cause.is_a?(Risp::Exception))
            puts ex
          else
            show_original_cause.call(ex.cause)
          end
        end
        show_original_cause.call(ex)
      end
    end
  end

  def self.parse(string)
    source = Lexer.new(string)
    parse_sexp(source).tap do |blah|
      token = source.next
      if token != :eof
        raise Risp::Exception.new("Expected EOF, got #{token}")
      end
    end
  rescue StopIteration
    raise Risp::Exception.new("Incomplete expression")
  end

  def self.parse_sexp(source)
    token = source.next
    special = parse_special(token, source)
    if special
      special
    else
      case token
      when "("
        parse_list(source, Risp::Qnil)
      else
        Risp::Atom.new(token)
      end
    end
  end

  def self.parse_special(token, source)
    case token
    when "'"
      Risp::to_list(
        Risp::Qquote,
        parse_sexp(source))
    when "`"
      Risp::to_list(
        Risp::Qquasiquote,
        parse_sexp(source))
    when ","
      Risp::to_list(
        Risp::Qunquote,
        parse_sexp(source))
    when ",@"
      Risp::to_list(
        Risp::Qunquote_splicing,
        parse_sexp(source))
    when "@"
      Risp::to_list(
        Risp::Qinspect,
        parse_sexp(source))
    when "!"
      Risp::to_list(
        Risp::Qpry,
        parse_sexp(source))
    end
  end

  def self.parse_list(source, list)
    token = source.next
    special = parse_special(token, source)
    if special
      parse_list(source, Risp::cons(special, list))
    else
      case token
      when "("
        sublist = parse_list(source, Risp::Qnil)
        parse_list(source, Risp::cons(sublist, list))
      when ")"
        Risp::reverse(list)
      else
        atom = Risp::Atom.new(token)
        parse_list(source, Risp::cons(atom, list))
      end
    end
  end

  class Lexer
    def self.new(string)
      Enumerator.new do |y|
        # This relies on regexp taking the first match not the longest match.
        string.scan(/\s*(,@|[()',`@!]|[^()\s]+)/) do |(token)|
          y << token
        end
        y << :eof
      end
    end
  end
end
