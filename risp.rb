#!/usr/bin/env ruby

require "hamster/hash"
require "byebug"

module Risp
  module Atom
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
        raise "No binding for #{self}"
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
    def initialize(&block)
      @block = block
    end

    def eval(args, bindings)
      @block.call(args, bindings)
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

  Qt = Symbol.new("t")
  global(Qt, Qt)

  def self.fsubr(name, f_name = name, &block)
    global(Symbol.intern(name), Fsubr.new(&block))
    define_singleton_method(f_name.to_sym, &block)
  end

  def self.subr(name, f_name = name, &block)
    global(Symbol.intern(name), Subr.new(&block))
    define_singleton_method(f_name.to_sym, &block)
  end

  fsubr("quote") do |args|
    args
  end
  
  subr("eq") do |args|
    (x, y) = to_array(args(2))
    to_boolean(x.is_a?(Atom) && y.is_a?(Atom) && x.eq(y))
  end

  subr("car") do |args|
    if args.is_a?(Cell)
      args.car
    else
      raise "Bad arg to car: #{args}"
    end
  end

  subr("cdr") do |args|
    if args.is_a?(Cell)
      args.cdr
    else
      raise "Bad arg to cdr: #{args}"
    end
  end

  # XXX the args will already be a cons.
  subr("cons") do |args|
    (x, y) = to_array(args, 2)
    Cell.new(x, y)
  end

  fsubr("cond") do |form, bindings|
    case
    when form == Qnil
      Qnil
    when eval(car(car(form)), bindings) != Qnil
      eval(cdr(car(form)), bindings)
    else
      cond(cdr(form), bindings)
    end
  end

  fsubr("lambda") do |args, bindings|
    symbol_array = to_array(car(args))
    form = cdr(args)
    Closure.new(symbol_array, form, bindings)
  end

  class Closure
    def initialize(symbol_array, form, bindings)
      @symbol_array = symbol_array
      @form = form
      @bindings = bindings
    end

    def eval(arg_list)
      args = to_array(arg_list, @symbol_array.length)
      bindings = @symbol_array.zip(args).reduce(@bindings) do |memo, (symbol, val)|
        memo.bind(symbol, val)
      end
      eval(@form, bindings)
    end

    def to_s
      @symbol_array.map(:to_s).join(" ") + " => " + @form.to_s
    end
  end

  def self.to_boolean(arg)
    arg ? Qt : Qnil
  end

  subr("null?") do |args|
    to_boolean(args == Qnil)
  end

  subr("atom?") do |args|
    to_boolean(args.is_a?(Atom))
  end

  subr("cons?") do |args|
    to_boolean(args.is_a?(Cell))
  end

  subr("list?") do |args|
    to_boolean(args == Qnil || args.is_a?(Cell))
  end

  def self.to_array(list, length = nil)
    init_list = list
    [].tap do |result|
      while cons?(list) == Qt
        result << car(list)
        list = cdr(list)
      end
      if list != Qnil
        raise "Expected proper list, got #{init_list}"
      end
      if length && result.length != length
        raise "Expected #{length} arguments, got #{result.length}"
      end
    end
  end

  subr("apply") do |args, bindings|
    (fn, fn_args) = to_array(args, 2)
    case fn
    when Fsubr
      fn.eval(fn_args, bindings)
    when Subr
      fn.eval(eval_list(fn_args, bindings), bindings)
    when Closure
      fn.eval(eval_list(fn_args, bindings))
    else
      raise "Don't know how to apply #{fn}"
    end
  end

  subr("eval") do |expr, bindings|
    case expr
    when Atom
      expr.eval(bindings)
    when Cell
      fn = eval(expr.car, bindings)
      args = expr.cdr
      apply(Cell.new(fn, Cell.new(args, Qnil)), bindings)
    else
      raise "Don't know how to eval #{expr}"
    end
  end

  fsubr("and", :f_and) do |args, bindings|
    case
    when args == Qnil
      Qnil
    when args.is_a?(Cell)
      if eval(args.car, bindings) == Qnil
        Qnil
      else
        f_and(args.cdr, bindings)
      end
    else
      raise "Can't do \"and\" with #{args}"
    end
  end

  fsubr("or", :f_or) do |args, bindings|
    case
    when args == Qnil
      Qt
    when args.is_a?(Cell)
      if eval(args.car, bindings) == Qt
        Qt
      else
        f_or(args.cdr, bindings)
      end
    else
      raise "Can't apply \"and\" to #{args}"
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
        raise "Can't apply \"+\" to #{args.car}"
      end
    else
      raise "Can't apply \"+\" to #{args}"
    end
  end

  def self.map_block(list, &block)
    if list == Qnil
      Qnil
    else
      Cell.new(block.call(car(list)), map_block(cdr(list), &block))
    end
  end

  def self.eval_list(list, bindings)
    map_block(list) do |element|
      eval(element, bindings)
    end
  end

  subr("map") do |args, bindings|
    (fn, list) = to_array(args, 2)
    map_block(list) do |element|
      apply(Cell.new(fn, Cell.new(element, Qnil)), bindings)
    end
  end
end

class Repl
  def self.parse(string)
    source = Lexer.new(string)
    token = source.next
    result =
      case token
      when "("
        parse_list(source, Risp::Qnil)
      when /^[0-9]+$/
        Risp::Number.new(token)
      else
        Risp::Symbol.intern(token)
      end
    token = source.next
    if token != :eof
      raise "Expected EOF, got #{token}"
    end
    result
  end

  def self.parse_list(source, list)
    token = source.next
    case token
    when "("
      sublist = parse_list(source, Risp::Qnil)
      parse_list(source, Risp::Cell.new(sublist, list))
    when ")"
      reverse(list)
    when /^[0-9]+$/
      number = Risp::Number.new(token)
      parse_list(source, Risp::Cell.new(number, list))
    else
      symbol = Risp::Symbol.intern(token)
      parse_list(source, Risp::Cell.new(symbol, list))
    end
  end

  def self.reverse(list, result = Risp::Qnil)
    if list == Risp::Qnil
      result
    else
      reverse(list.cdr, Risp::Cell.new(list.car, result))
    end
  end

  class Lexer
    def self.new(string)
      Enumerator.new do |y|
        string.scan(/\s*(\(|\)|[^()\s]+)/) do |(token)|
          y << token
        end
        y << :eof
      end
    end
  end
end

