# RISP

This is a little lisp interpreter I put together to test the ideas in
the paper "Cons should not evaluate its arguments", in which `cons`
was modified to put "suspensions" in the `car` and `cdr` of newly
allocated cells and `car`/`cdr` would evaluate the suspensions.  The
hope being to get lazy evaluation.

This dialect is a mish-mash of scheme and emacs-lisp.  It is lexically
scoped and uses `define` and `null?` but it has `t` and `nil` atoms
and `cond` has no "else" clause.  There's probably more mish-mash as
well.

Well.  Despite proving that their interpreter is strictly more
powerful than McCarthy's interpreter because it can evaluate some
things that would cause McCarthy to diverge, it turns out that "Cons
should not evaluate its arguments" is not powerful enough, and that
...

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

## What's wrong with cons whould not evaluate its arguments?

It's just not the right place to create thunks.  It only creates
thunks on `cons`.  But not everything `cons`es.  A lot of things work,
but some didn't.

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

The `filter` should produce `(other)` and the result should be
`((1 other))`.  But `filter`:

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
code:

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