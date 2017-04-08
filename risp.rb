#!/usr/bin/env ruby

require "hamster/hash"
require "readline"
require "byebug"

at_exit do
  Lepr.repl
end

module Risp
  module Atom
    def self.new(string)
      case string
      when /^[0-9]+$/
        Risp::Number.new(string)
      else
        Risp::Symbol.intern(string)
      end
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
      bindings.get(self) || Risp.global_bindings.get(self) or
        raise Risp::Exception.new("No binding for #{self}")
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

    def initialize(string)
      @val = string.to_i
    end

    def val
      @val
    end

    def eval(bindings)
      self
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

    def to_s(ch = "(")
      ch + 
        if car.is_a?(Cell)
          car.to_s("(")
        else
          car.to_s
        end +
        case
        when cdr == Risp::Qnil
          ")"
        when cdr.is_a?(Cell)
          cdr.to_s(" ")
        else
          " . " + cdr.to_s + ")"
        end
    end
  end

  class Closure
    def initialize(symbol_array, form, bindings)
      @symbol_array = symbol_array
      @form = form
      @bindings = bindings
    end

    def eval(arg_list)
      args = Risp::to_array(arg_list, @symbol_array.length)
      bindings = @symbol_array.zip(args).reduce(@bindings) do |memo, (symbol, val)|
        memo.bind(symbol, val)
      end
      Risp.eval(@form, bindings)
    end

    def to_s
      @symbol_array.map(&:to_s).join(" ") + " => " + @form.to_s
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
      if @nargs
        array = Risp::to_array(args, @nargs)
        @block.call(*array, bindings)
      else
        @block.call(args, bindings)
      end
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
      raise Risp::Exception.new("Bad arg to car: #{arg}")
    end
  end

  subr("cdr", 1) do |arg|
    if arg.is_a?(Cell)
      arg.cdr
    else
      raise Risp::Exception.new("Bad arg to cdr: #{arg}")
    end
  end

  subr("cons", 2) do |x, y|
    Cell.new(x, y)
  end

  fsubr("cond", 1) do |form, bindings|
    case
    when form == Qnil
      Qnil
    when eval(car(car(form)), bindings) != Qnil
      eval(car(cdr(car(form))), bindings)
    else
      cond(cdr(form), bindings)
    end
  end

  fsubr("lambda", 2) do |symbols, form, bindings|
    symbol_array = to_array(symbols)
    Closure.new(symbol_array, form, bindings)
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
      raise Risp::Exception.new("Don't know how to apply #{fn}")
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
      raise Risp::Exception.new("Don't know how to eval #{expr}")
    end
  end

  fsubr("and", nil, :f_and) do |args, bindings|
    case
    when args == Qnil
      Qt
    when args.is_a?(Cell)
      if eval(args.car, bindings) == Qnil
        Qnil
      else
        f_and(args.cdr, bindings)
      end
    else
      raise Risp::Exception.new("Can't do \"and\" with #{args}")
    end
  end

  fsubr("or", nil, :f_or) do |args, bindings|
    case
    when args == Qnil
      Qnil
    when args.is_a?(Cell)
      if eval(args.car, bindings) == Qt
        Qt
      else
        f_or(args.cdr, bindings)
      end
    else
      raise Risp::Exception.new("Can't apply \"and\" to #{args}")
    end
  end

  subr("+") do |args|
    case
    when args == Qnil
      0
    when args.is_a?(Cell)
      if args.car.is_a?(Number)
        args.car.val + send(:+, args.cdr)
      else
        raise Risp::Exception.new("Can't apply \"+\" to #{args.car}")
      end
    else
      raise Risp::Exception.new("Can't apply \"+\" to #{args}")
    end
  end

  subr("map", 2) do |fn, list, bindings|
    map_block(list) do |element|
      apply(fn, cons(element, Qnil), bindings)
    end
  end

  def self.to_boolean(arg)
    arg ? Qt : Qnil
  end

  def self.to_array(list, length = nil)
    init_list = list
    [].tap do |result|
      while cons?(list) == Qt
        result << car(list)
        list = cdr(list)
      end
      if list != Qnil
        raise Risp::Exception.new("Expected proper list, got #{init_list}")
      end
      if length && result.length != length
        raise Risp::Exception.new("Expected #{length} arguments, got #{result.length}")
      end
    end
  end

  def self.map_block(list, &block)
    if list == Qnil
      Qnil
    else
      cons(block.call(car(list)), map_block(cdr(list), &block))
    end
  end

  def self.eval_list(list, bindings)
    map_block(list) do |element|
      eval(element, bindings)
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
        puts Risp.eval(expr).to_s
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
      to_list(Risp::Symbol.intern("quote"),
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
      quoted = to_list(Risp::Symbol.intern("quote"), parse_expr(source))
      parse_list(source, Risp::cons(quoted, list))
    when "("
      sublist = parse_list(source, Risp::Qnil)
      parse_list(source, Risp::cons(sublist, list))
    when ")"
      reverse(list)
    else
      atom = Risp::Atom.new(token)
      parse_list(source, Risp::cons(atom, list))
    end
  end

  def self.to_list(*elements)
    elements.reverse.reduce(Risp::Qnil) do |memo, element|
      Risp::cons(element, memo)
    end
  end

  def self.reverse(list, result = Risp::Qnil)
    if list == Risp::Qnil
      result
    else
      reverse(list.cdr, Risp::cons(list.car, result))
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
