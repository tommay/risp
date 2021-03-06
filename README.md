# RISP

This is a little lisp interpreter I put together to test the ideas in
the paper "Cons should not evaluate its arguments", in which `cons`
was modified to put "suspensions" in the `car` and `cdr` of newly
allocated cells and `car`/`cdr` would evaluate the suspensions.  The
hope being to get lazy evaluation.

I found out that doesn't make everything lazy because not everything
is in a `cons` cell and accessed with `car` and `cdr`.  It should be
"Eval should not evaluate its argument".

## Miscelleous

This lisp dialect is a mish-mash of scheme and emacs-lisp.  It is
lexically scoped and uses `define` and `null?` but it has `t` and
`nil` atoms and `cond` has no "else" clause.  There's probably more
mish-mash as well.  Doing everything the scheme way would be better
but this was faster/easier to write.

Variables are immutable, i.e., there are no side-effect functions
other than define and define-macro which define top-level variables.
Top-level variables can be redefined.

Well.  Despite proving that their interpreter is strictly more
powerful than McCarthy's interpreter because it can evaluate some
things that would cause McCarthy to diverge, it turns out that "Cons
should not evaluate its arguments" is not powerful enough.  What seems
to be needed is the idea in the next section.

## Eval should not evaluate its arguments

On the master branch is an interpreter written from my own
understanding of how a lexically scoped lisp interpreter should work.

And then I made `eval` return thunks containing its `form` and
`bindings` arguments instead of evaluating `form`.  The thunks are
evaluated later when the evaluated value is actually needed, e.g., to
print out, to check for `null?`, to add numbers, etc.

This works great.  The only (I think) problem being ruby's (MRI 2.4.0)
lack of tail-call optimization so trying to print an infinite list,
even though it's done incrementally, will blow the stack.

Update: I've tried using trampolines to do tail recursion without
blowing the stack and they don't seem to fix the memory problem.
Perhaps it has to do with risp creating an evaluation tree instead of
an evaluation graph.  Somewhere here there is risp, Haskell, and Frege
code where both risp and Frege have the memory problem but Haskell
does not, IIRC it's a zip/filter example but I could be wrong.  That's
a good place to start looking into things.

Or consider this code:
ones = 1 : ones
(define ones (cons 1 ones))

In risp, evaluating the tree will create a new Thunk with new bindings
for each invocation of "ones".

## What's wrong with cons should not evaluate its arguments?

It's just not the right place to create thunks.  It only creates
thunks on `cons`.  But not everything `cons`es.  A lot of things work,
but some don't.

Consider an infinite list of atoms:

~~~~
(define (atoms) (cons 'atom (atoms)))
~~~~

This worked fine:

~~~~
(zip '(1) (atoms)
=> ((1 atom))
~~~~

But this blew the stack:

~~~~
(zip '(1) (filter (lambda (n) (eq n 'other)) (cons 'other (atoms))))
~~~~

The `filter` should produce `(other ...)` and the result should be
`((1 other))`.  Since the first list has only one element, only one element
from the second list should be retrieved.  But `filter`, which is:

~~~~
(define (filter pred lst)
  (cond
   ((null? lst) nil)
   ((pred (car lst))
    (cons (car lst) (filter fn (cdr lst))))
   (t
    (filter fn (cdr lst)))))
~~~~

would return the first `cons` then get into a loop where it never
called `cons` again and therefore would never return.

Since risp.rb in the cons-does-not-evaluate branch is very close to
McCarthy's lisp, it doesn't have `define` and uses `atom` to test for
empty list, and it has dynamic scoping.  Here's the actual failing
code for that branch where zip, filter, and atoms are created as
lambda expressions that call themselves recursively through their
dynamically scoped bindings then are passed into a lambda that does
the actual zip/filter expression:

~~~~
((lambda (zip filter atoms)
     (zip '(1) (filter (lambda (z) (eq z 'other)) (cons 'other (atoms)))))
   (lambda (a b)
     (cond
       ((atom a) nil)
       ((atom b) nil)
       (t (cons (cons (car a) (cons (car b) nil)) (zip (cdr a) (cdr b))))))
   (lambda (fn lst)
     (cond
       ((atom lst) nil)
       ((fn (car lst)) (cons (car lst) (filter fn (cdr lst))))
       (t (filter fn (cdr lst)))))
   (lambda () (cons 'atom (atoms))))
~~~~

## On divergence

A lot of things that diverge will blow the stack instead of just
running forever.  For example, this runs correctly:

~~~~
(define evens
  (cons 0 (map (lambda (a) (+ a 2)) evens)))

(zip '(a) (filter (lambda (n) (eq n 4)) evens))
=> ((a 4))
~~~~

but if the arguments to zip are reversed then zip diverges since
filtering an infinite list gives an infinite list and zip will never
terminate, even though it would be fine if it did since the second
list is finite.  However, it will blow the stack.

Haskell also diverges in the same case, but doesn't blow the stack:

~~~~
evens = map (+2) (0 : evens)
zip ["a"] (filter (== 4) evens)
=> [("a",4)]
zip (filter (== 4) evens) ["a"]
=> [(4,"a")  -- then runs forever
~~~~

I think blowing the stack is due to a lack of tail-call optimization.
I could possibly use trampolines to get around this.

Update: I tried trampolines (commit 8fb6c578187beb89baeb0865a26845b81e98bd7e) and that didn't help.  It may be an issue of expression tree vs. expression graph.

## Problems

### Some things that don't diverge blow the stack:

All of these blow the stack.

~~~~
(load 'numbers)
(nth 1000000000 ones)
(nth 1000 numbers1)
(nth 1000 numbers1a)
(nth 1000 (numbers 1))
~~~~

I've used a trampoline so arbitrarily long thunk/memo chains can be
dethunked but the problem is more insidious.  The thunks/bindings become
arbitrarily large.

### Thunk memos blow the heap

Thunk memos can make arbitrarily long chains and blow the heap because
they are strongly referenced.  `WeakRef` doesn't help because the
`WeakRef`s are aggressively garbage collected and don't live long enough
to be effective.  SoftReferences are what's needed.  See commit
6f9bd3a268fa8afd0837fc8cbdf8c1932bbd825f.

Maybe do it in jruby and use java's SoftReferences.

### Blowing the heap but not the stack

This will use arbitrary heap but limited stack:

~~~~
(nth 1000000 ones)
~~~~

This will blow the stack:

~~~~
(apply + (take 10000 ones))
~~~~

Using the Y-combinator version, `yones`, has the same limitations.

### `and`/`or` should iterate or use trampolines

If/when everything works nicely and infinite lists don't cause
problems, `and` and `or` should be changed from tail recursion
to iteration or trampolines so they can handle arbitrarily long
lists such as `(all? ...)` or `(any? ...)` might want.

Update: this has been done in commit be3a47a475a49c6434a2133b75b88860b4a827eb.
