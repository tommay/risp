#!/usr/bin/env ruby

require "bundler/setup"
require "hamster/hash"
require "readline"
require "readline/history/restore"
require "trollop"
require "pry-byebug"

Readline::History::Restore.new(File.expand_path("~/.risp_history"))

module Risp
  @options = Trollop::options do
    banner <<EOS
Usage: #{$0} [options] [file...]
Run the risp repl to interpret my tiny lisp dialect with trict evaluation.
EOS
    opt :trace, "Print evaluation trace"
  end

  def self.do_trace
    @options.trace
  end

  at_exit do
    execute("prelude.risp", true)
    if ARGV.size != 0
      ARGV.each do |file|
        execute(file, true)
      end
    else
      ::Lepr.repl
    end
  end

  def self.execute(file, do_print)
    file = File.read(file)
    no_comments = file.gsub(/;.*/, "")
    no_comments.split(/\n{2,}/).each do |section|
      if section !~ /^\s*$/
        eval(::Lepr.parse(section)).tap do |val|
          if do_print
            if do_trace
              puts val.to_s
            else
              val.print
              puts
            end
          end
        end
      end
    end
  end

  if do_trace
    @indent = ""
    def self.trace(string_proc, &block)
      indent = @indent
      puts "#{indent}#{string_proc.call} {"
      @indent += "  "
      begin
        block.call.tap do |result|
          puts "#{indent}} => #{result.inspect}"
        end
      ensure
        @indent = indent
      end
    end
  else
    def self.trace(string_proc, &block)
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

    def inspect
      to_s
    end

    def print
      Kernel.print(to_s)
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

    def eval(bindings)
      Risp.trace(lambda{"#{name}"}) do
        bindings.get(self) || Risp.global_bindings.get(self) or
          raise Risp::Exception.new("No binding for #{self.inspect}")
      end
    end

    def eq(other)
      other.is_a?(Symbol) && self.name == other.name
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
          raise "Bad arg to Number.new: #{thing.inspect}"
        end
    end

    def val
      @val
    end

    def eval(bindings)
      self
    end

    def +(other)
      if other.is_a?(Number)
        Number.new(self.val + other.val)
      else
        raise Risp::Exception.new("Can't apply \"+\" to #{other.inspect}")
      end
    end

    def -(other)
      if other.is_a?(Number)
        Number.new(self.val - other.val)
      else
        raise Risp::Exception.new("Can't apply \"-\" to #{other.inspect}")
      end
    end

    def *(other)
      if other.is_a?(Number)
        Number.new(self.val * other.val)
      else
        raise Risp::Exception.new("Can't apply \"*\" to #{other.inspect}")
      end
    end

    def eq(other)
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

    def inspect(ch = "(")
      ch + 
        if @car.is_a?(Cell)
          @car.inspect("(")
        else
          @car.inspect
        end +
        case
        when @cdr == Risp::Qnil
          ")"
        when @cdr.is_a?(Cell)
          @cdr.inspect(" ")
        else
          " . " + @cdr.inspect + ")"
        end
    end

    def to_s(ch = "(")
      a = Risp.car(self)
      ca =
        if a.is_a?(Cell)
          a.to_s("(")
        else
          a.to_s
        end

      d = Risp.cdr(self)
      cd =
        case
        when d == Risp::Qnil
          ")"
        when d.is_a?(Cell)
          d.to_s(" ")
        else
          " . " + d.to_s + ")"
        end

      ch + ca + cd
    end

    def print(ch = "(")
      Kernel.print(ch)

      a = Risp.car(self)
      if a.is_a?(Cell)
        a.print("(")
      else
        a.print
      end

      d = Risp.cdr(self)
      case
      when d == Risp::Qnil
        Kernel.print(")")
      when d.is_a?(Cell)
        d.print(" ")
      else
        Kernel.print(" . ")
        d.print
        Kernel.print(")")
      end
    end
  end

  class Closure
    def initialize(symbol_array, form, bindings, name)
      @symbol_array = symbol_array
      @form = form
      @bindings = bindings
      @name = name || form.inspect
    end

    def eval(arg_list)
      Risp.trace(lambda{"#{@name}#{arg_list.inspect}"}) do
        args = Risp::to_array(arg_list, @symbol_array.length)
        bindings = @symbol_array.zip(args).reduce(@bindings) do |memo, (symbol, val)|
          memo.bind(symbol, val)
        end
        Risp.eval(@form, bindings)
      end
    end

    def inspect
      to_s(:inspect)
    end

    def print
      super("[(" + @symbol_array.map(&:to_s).join(" ") + ") => ")
      @form.print
      super("]")
    end

    def to_s(method = :to_s)
      "[(" + @symbol_array.map(&method).join(" ") + ") => " +
        @form.send(method) + "]"
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

    def inspect
      vals = @hash.flatten(0).map do |k, v|
        "#{k.inspect}: #{v.inspect}"
      end
      "{" + vals.join(", ") + "}"
    end

    # This is for letrec where we have to set the value after the
    # binding is created.
    class Slot
      attr_accessor :val
    end
  end

  module Builtin
    def initialize(name, nargs, &block)
      @name = name
      @nargs = nargs
      @block = block
    end

    def eval(args, bindings)
      Risp.trace(lambda{"#{@name}#{args.inspect}"}) do
        if @nargs
          array = Risp::to_array(args, @nargs)
          @block.call(*array, bindings)
        else
          @block.call(args, bindings)
        end
      end
    end

    def inspect
      to_s
    end

    def print
      super(to_s)
    end

    def to_s
      "<#{self.class}: #{@name}>"
    end
  end

  class Subr
    include Builtin
  end

  class Fsubr
    include Builtin
  end

  @global_bindings = Bindings.new

  def self.global_bindings
    @global_bindings
  end

  def self.global(symbol, val)
    @global_bindings = @global_bindings.bind(symbol, val)
  end

  Qnil = Symbol.intern("nil")
  global(Qnil, Qnil)

  Qt = Symbol.intern("t")
  global(Qt, Qt)

  def self.fsubr(name, nargs = nil, f_name = name, &block)
    global(Symbol.intern(name), Fsubr.new(name, nargs, &block))
    define_singleton_method(f_name.to_sym, &block)
  end

  def self.subr(name, nargs = nil, f_name = name, &block)
    global(Symbol.intern(name), Subr.new(name, nargs, &block))
    define_singleton_method(f_name.to_sym, &block)
  end

  fsubr("quote", 1) do |arg|
    arg
  end
  
  subr("eq", 2) do |x, y|
    to_boolean(x.is_a?(Atom) && y.is_a?(Atom) && x.eq(y))
  end

  subr("car", 1) do |arg|
    if arg.is_a?(Cell)
      arg.car
    else
      raise Risp::Exception.new("Bad arg to car: #{arg.inspect}")
    end
  end

  subr("cdr", 1) do |arg|
    if arg.is_a?(Cell)
      arg.cdr
    else
      raise Risp::Exception.new("Bad arg to cdr: #{arg.inspect}")
    end
  end

  subr("cons", 2) do |x, y|
    Cell.new(x, y)
  end

  fsubr("cond") do |form, bindings|
    case
    when form == Qnil
      Qnil
    when eval(car(car(form)), bindings) != Qnil
      eval(car(cdr(car(form))), bindings)
    else
      cond(cdr(form), bindings)
    end
  end

  fsubr("lambda", 2) do |symbols, form, bindings, name = nil|
    symbol_array = to_array(symbols)
    check_symbols(symbol_array)
    Closure.new(symbol_array, form, bindings, name)
  end

  subr("null?", 1) do |arg|
    to_boolean(arg == Qnil)
  end

  subr("atom?", 1) do |arg|
    to_boolean(arg.is_a?(Atom))
  end

  subr("cons?", 1) do |arg|
    to_boolean(arg.is_a?(Cell))
  end

  subr("list?", 1) do |arg|
    to_boolean(arg == Qnil || arg.is_a?(Cell))
  end

  subr("apply", 2) do |fn, args, bindings|
    case fn
    when Fsubr
      fn.eval(args, bindings)
    when Subr
      fn.eval(eval_list(args, bindings), bindings)
    when Closure
      fn.eval(eval_list(args, bindings))
    else
      raise Risp::Exception.new("Don't know how to apply #{fn.inspect}")
    end
  end

  subr("eval", 1) do |expr, bindings = Bindings.new|
    case expr
    when Atom
      expr.eval(bindings)
    when Cell
      fn = eval(expr.car, bindings)
      args = expr.cdr
      apply(fn, args, bindings)
    else
      raise Risp::Exception.new("Don't know how to eval #{expr.inspect}")
    end
  end

  fsubr("and", nil, :f_and) do |args, bindings|
    case
    when args == Qnil
      Qt
    when args.is_a?(Cell)
      val = eval(args.car, bindings)
      case
      when val == Qnil
        Qnil
      when args.cdr == Qnil
        val
      else
        f_and(args.cdr, bindings)
      end
    else
      raise Risp::Exception.new("Can't do \"and\" with #{args.inspecct}")
    end
  end

  fsubr("or", nil, :f_or) do |args, bindings|
    case
    when args == Qnil
      Qnil
    when args.is_a?(Cell)
      val = eval(args.car, bindings)
      if val != Qnil
        val
      else
        f_or(args.cdr, bindings)
      end
    else
      raise Risp::Exception.new("Can't apply \"and\" to #{args.inspect}")
    end
  end

  subr("+") do |list|
    fold_block(Number.new(0), list) do |memo, arg|
      memo + arg
    end
  end

  subr("-") do |list|
    case list
    when Cell
      val = list.car
      case val
      when Number
        if list.cdr == Qnil
          Number.new(-val.val)
        else
          fold_block(val, list.cdr) do |memo, arg|
            memo - arg
          end
        end
      else
        raise Risp::Exception.new("Can't apply \"-\" to #{val.inspect}")
      end
    else
      raise Risp::Exception.new("Can't apply \"-\" to #{list.inspect}")
    end
  end

  subr("*") do |list|
    fold_block(Number.new(1), list) do |memo, arg|
      memo * arg
    end
  end

  subr("list", nil) do |list|
    list
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
    case args
    when Symbol
      eval(form, bindings).tap do |val|
        global(args, val)
      end
    when Cell
      name = args.car
      symbols = args.cdr
      lambda(symbols, form, bindings, name.to_s).tap do |closure|
        global(name, closure)
      end
    else
      raise Risp::Exception.new("Can't define #{args.inspect}")
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
        raise Risp::Exception.new("Expected #{length} arguments, got #{array.length}")
      end
    end
  end

  def self.fold_block(memo, list, &block)
    case list
    when Qnil
      memo
    when Cell
      new_memo = block.call(memo, list.car)
      fold_block(new_memo, list.cdr, &block)
    else
      raise Risp::Exception.new("Can't fold #{list.inspect}")
    end
  end

  def self.map_block(list, &block)
    case list
    when Qnil
      Qnil
    when Cell
      cons(block.call(list.car), map_block(list.cdr, &block))
    else
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
      raise Risp::Exception.new("Not a symbol: #{nonsymbols.first.inspect}")
    else
      raise Risp::Exception.new("Not symbols: #{nonsymbols.map(&:inspect).join(" ")}")
    end
  end

  class Exception < ::Exception
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
        expr = parse(line)
        Risp.eval(expr).tap do |val|
          if Risp.do_trace
            puts val.to_s
          else
            val.print
            puts
          end
        end
      rescue Risp::Exception => ex
        puts ex
      end
    end
  end

  def self.parse(string)
    source = Lexer.new(string)
    parse_expr(source).tap do |blah|
      token = source.next
      if token != :eof
        raise Risp::Exception.new("Expected EOF, got #{token}")
      end
    end
  end

  def self.parse_expr(source)
    token = source.next
    case token
    when "'"
      Risp::to_list(
        Risp::Symbol.intern("quote"),
        parse_expr(source))
    when "("
      parse_list(source, Risp::Qnil)
    else
      Risp::Atom.new(token)
    end
  end

  def self.parse_list(source, list)
    token = source.next
    case token
    when "'"
      quoted = Risp::to_list(
        Risp::Symbol.intern("quote"),
        parse_expr(source))
      parse_list(source, Risp::cons(quoted, list))
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

  class Lexer
    def self.new(string)
      Enumerator.new do |y|
        string.scan(/\s*([()']|[^()'\s]+)/) do |(token)|
          y << token
        end
        y << :eof
      end
    end
  end
end
