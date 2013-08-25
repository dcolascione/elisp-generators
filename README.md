elisp-generators
================

This repository contains an implementation of Python-style generators
for Emacs Lisp.  It requires Emacs 24.3 or better.

Introduction
------------

A generator is a kind of [limited
coroutine](http://en.wikipedia.org/wiki/Coroutine).  When a program
calls a generator function, the generator returns an iterator instead
of a value.  The code inside the generator routine is not run in this
case.  Instead, we run procedure code when users extract values from
the returned iterator.

A program extracts values from an iterator by passing the iterator to
the `next` function.  When an iterator can generate no more values,
`next` raises the stop-iteration condition, which callers must handle
using condition-case if not using a higher-level iteration construct.
`next` accepts a single argument that defaults to nil.  This argument
becomes the value to which `yield` evaluates in the context of the
generator.

Generators supply values using the special `yield` macro.  When
`yield` appears in a generator routine, it indicates that the iterator
we created when calling the generator routine should return that
value.  Execution of the generator code is suspended at the point of
`yield` until the next time someone calls the iterator.  If nobody
calls `next`, execution never resumes.

Each iterator gets its own control flow state and lexical environment.
If one calls a generator function five times, the result is five
independent iterators.

Dynamic environment
-------------------

Generators do not capture the values of the current buffer, dynamic
variables, and so on before they return an iterator.  From the point
of view of a generator, any state not lexically bound may appear to
change arbitrarily across a call to `yield`.  If generators need a
consistent environment while they generate values, they should
contractually require that callers preserve this environment or save
the environment themselves the way that a process filter might.

Allowed code
------------

Generator definitions need to occur in files that make lexical-binding
true.  Code that merely _uses_ a generator, however, can work with
lexical-binding true or false.

Almost any Emacs Lisp code can appear inside a generator.  One can
yield from inside a loop, inside a catch statement, or in any other
context.  A generator can even generate an infinite sequence.  `yield`
is legal inside `catch`, inside `condition-case`, and in almost any
other context.

There is one exception: generator routines may not yield is the
unwindforms portion of an `unwind-protect`: it is not legal to yield
from inside one of these forms because the Lisp runtime does not give
us enough information to continue unwinding (in a generic way) after
suspending execution to return the yielded value from the iterator.
Note that is _is_ legal to yield inside the bodyform (i.e., the first
form) of an `unwind-protect`.

Simple usage
------------

    (assert lexical-binding)
    (defgenerator mygenerator (i)
      (yield 1)
      (yield i)
      (yield 2))

    (let ((it (mygenerator 42)))
      (assert (eql (next it) 1))
      (assert (eql (next it) 42))
      (assert (eql (next it) 2))
      (assert (eq (condition-case nil
                      (next it)
                    (stop-iteration 'foo))
                  'foo)))

Convenience facilities
----------------------

It would be inconvenient to write `next` and `condition-case` in every
context in which one might want to extract values from an iterator.
This library supplies a few convenience functions that make it easier
to work with iterators.

### do-iterator ###

`do-iterator` works like `dolist`, except that it takes values from an
iterator, not a list.

    (let (mylist)
      (do-iterator (x (mygenerator 4))
        (push x mylist))

      (assert (equal mylist '(2 4 1))))


### loop / cl-loop ###

This package also extends `loop` with a new iteration keyword,
`iterating`, that causes `loop` to draw values from an iterator.  This
keyword works the same way that `on` and `across` do.

    (assert (equal (loop for x iterating (mygenerator 42)
                         collect x)
                   '(1 42 2)))
