#!/usr/bin/env ruby

require "bundler/setup"
require "readline"
require "readline/history/restore"
require "pry-byebug"

# This is an ultra-primitive lisp based on the non-lazy interpeter
# given in "Cons should not evaluate its arguemnts."  It doesn't
# have numbers or top-level defines so it's not easy to play with.
# It does have a repl that understands ' and has really poor diagnostics.
#
# Quirks:
# - User-defined functions have to be done as lambdas bound to the
#   arguments of a main outer lambda.
# - Lambdas have to be quoted because lambda isn't a built-in that
#   returns its own form.
# - Unbound atoms evaluate to nil.
# - No atoms are initially bound therefore nil luckily evaluates to nil.
# - Unknown functions return nil instead of erroring.
# - Numbers and arithmetic are not supported.
# - (atom ...) must be used to test for the end of a list, i.e., nil.
# - I didn't make it so cond has a final else clause.  And since there
#   is no atom t that evaluates to itself, use ('t ...) in the final
#   cond pair.
#
# But it can evaluate an expression like this, which defines a map
# function and uses it to map another function over a list and return
# the result.  The function that gets mapped here just wraps its argument
# in a list by consing it onto nil:
#
#   ((lambda (map lst)
#      (map '(lambda (e) (cons e nil)) lst))
#     '(lambda (fn lst)
#        (cond ((atom lst) nil) ('t (cons (fn (car lst)) (map fn (cdr lst))))))
#     '(a b c d e))
#   => ((a) (b) (c) ((d e)) (f))
#
# Here's another example that defines zip and zips two lists:
#   ((lambda (zip)
#        (zip '(1 2 3) '(a b c d e)))
#      '(lambda (a b)
#         (cond
#           ((atom a) nil)
#           ((atom b) nil)
#           ('t (cons (cons (car a) (cons (car b) nil))
#                     (zip (cdr a) (cdr b)))))))
#   => ((1 a) (2 b) (3 c))
#
# Note that these only work because of the dynamic scoping.  Lexically,
# the lambdas that are bound to map and zip wouldn't have access to map/zip's
# bindings so they couldn't recurse via those names.
#
# The methods _cons, _car, and _cdr are the fundammental McCarthy
# functions that work with Cells.  The methods car, cdr, and scons
# are used by the interpeter, e.g., (car ...) uses method car, and
# will be redefined to create the lazy interpreter.

Readline::History::Restore.new(File.expand_path("~/.risp_history"))

at_exit do
  Lepr.repl
end

module Risp
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

  module Atom
    def self.new(string)
      Risp::Symbol.intern(string)
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
        bindings.get(self) or
          raise Risp::Exception.new("No binding for #{self}")
      end
    end

    def to_s
      name
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

  Qnil = Symbol.intern("nil")
  Qt = Symbol.intern("t")

  CAR   = Symbol.intern("car")
  CDR   = Symbol.intern("cdr")
  CONS  = Symbol.intern("cons")
  EQ    = Symbol.intern("eq")
  ATOM  = Symbol.intern("atom")

  QUOTE  = Symbol.intern("quote")
  COND   = Symbol.intern("cond")
  LAMBDA = Symbol.intern("lambda")

  def self.eval(form, bindings = Qnil)
    case
    when atom(form) == Qt
      # XXX Make assoc raise an exception if there is no binding
      # instead of returning nil.  Then nil needs an explicit binding.
      # And make an error subr?
      assoc(form, bindings)
    # Not an atom, must be a list:
    when atom(car(form)) == Qt
      # These are the built-in fsubrs:
      case car(form)
      when QUOTE
        car(cdr(form))
      when CONS
        scons(cdr(form), bindings)
      when COND
        evcon(cdr(form), bindings)
      else
        # This is for subrs and user-defined functions.
        apply(car(form), evlis(cdr(form), bindings), bindings)
      end
    else
      # The thing in function position is a list.  It's either a lambda
      # or we raise an exception.
      apply(car(form), evlis(cdr(form), bindings), bindings)
    end
  end

  # Args have been evaluated.
  def self.apply(fn, args, bindings)
    case
    when atom(fn) == Qt
      # These are the built-in subrs:
      case fn
      when CAR
        car(_car(args))
      when CDR
        cdr(_car(args))
      when EQ
        eq(_car(args), _car(_cdr(args)))
      when ATOM
        atom(_car(args))
      when Qnil
        # Is this is a strange way to handle nil and make it evaluate
        # to itself.  Any args are ignored.  And nil evaluates to
        # itself anyway since it has no binding and looking up Qnil
        # will return Qnil.
        Qnil
      else
        # Look up the fn's binding and try again.
        apply(eval(fn, bindings), args, bindings)
      end
    when eq(car(fn), LAMBDA) == Qt
      # LAMBDA isn't a subr or fsubr, it's just a merker that lets us
      # know what to do.
      eval(car(cdr(cdr(fn))),
           pairlis(car(cdr(fn)), args, bindings))
    else
      raise Risp::Exception.new("Don't know how to apply #{fn.inspect}")
    end
  end

  def self.scons(ab, bindings)
    _cons(eval(_car(ab), bindings),
          eval(_car(_cdr(ab)), bindings))
  end
  
  def self._car(arg)
    case arg
    when Cell
      arg.car
    else
      raise Risp::Exception.new("Bad arg to car: #{arg.inspect}")
    end
  end

  def self._cdr(arg)
    case arg
    when Cell
      arg.cdr
    else
      raise Risp::Exception.new("Bad arg to cdr: #{arg.inspect}")
    end
  end

  def self._cons(x, y)
    Cell.new(x, y)
  end

  def self.car(arg)
    _car(arg)
  end

  def self.cdr(arg)
    _cdr(arg)
  end

  def self.eq(x, y)
    to_boolean(x == y)
  end

  # Note that Qnil is considered an atom, which is fine.
  def self.atom(x)
    to_boolean(x.is_a?(Atom))
  end

  def self.pairlis(fpl, apl, bindings)
    if fpl == Qnil
      bindings
    else
      _cons(_cons(car(fpl), _car(apl)),
            pairlis(cdr(fpl), _cdr(apl), bindings))
    end
  end

  def self.assoc(at, bindings)
    case
    when bindings == Qnil
      # XXX raise an exception?
      Qnil
    when eq(_car(_car(bindings)), at) == Qt
      _cdr(_car(bindings))
    else
      assoc(at, _cdr(bindings))
    end
  end

  def self.evlis(unargs, bindings)
    if unargs == Qnil
      Qnil
    else
      _cons(eval(car(unargs), bindings),
            evlis(cdr(unargs), bindings))
    end
  end

  def self.evcon(tail, bindings)
    case
    when tail == Qnil
      Qnil
    when eval(car(car(tail)), bindings) != Qnil
      eval(car(cdr(car(tail))), bindings)
    else
      evcon(cdr(tail), bindings)
    end
  end

  def self.to_boolean(arg)
    arg ? Qt : Qnil
  end

  def self.to_list(*elements)
    elements.reverse.reduce(Risp::Qnil) do |memo, element|
      _cons(element, memo)
    end
  end

  def self.fold_list(accum, list, &block)
    case list
    when Qnil
      accum
    when Cell
      accum = block.call(accum, _car(list))
      fold_list(accum, _cdr(list), &block)
    else
      raise Risp::Exception.new("Can't fold #{list.inspect}")
    end
  end

  def self.reverse(list)
    fold_list(Qnil, list) do |memo, element|
      _cons(element, memo)
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
        Risp.eval(expr).print
        puts
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
      parse_list(source, Risp::_cons(quoted, list))
    when "("
      sublist = parse_list(source, Risp::Qnil)
      parse_list(source, Risp::_cons(sublist, list))
    when ")"
      Risp::reverse(list)
    else
      atom = Risp::Atom.new(token)
      parse_list(source, Risp::_cons(atom, list))
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
