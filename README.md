# LowerDictPy

This implements a dictionary-lowering compilation pass.

In front of that pass, there's a small, typed functional language written using s-expressions.

After lowering is done, the result is sent to a simple tree-walking interpreter.

## Installing and testing

This depends on Python 3.

`make installdeps` to install the python tools used for development.

`make test` to test, `make check` to run some basic lints.

## Running

Run main.py, e.g.

    python3 ./main.py examples/using_superclasses_in_instance.lisp


The file run must contain a `main` function.

## Syntax example

```lisp
(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c)
  superclasses (Parent)
  (:: child (Fn c Int)))

(instance (Parent String)
  (fn parent (s)
    (:: ((:: length (Fn String Int)) (:: s String)) Int)))

(instance (Child String)
  (fn child (s)
    (::
      ((:: inc (Fn Int Int))
       (:: ((:: parent (Fn String Int)) (:: s String)) Int))
      Int)))
```

The nodes that start with `::` are defining the type of the first argument.
This is currently necessary because this has no type checker or type inference.


## Status

This has a parser front-end that uses an ml-like lisp-based syntax.

- Add a type check pass after the lowering is done to sanity-check the output (assuming this isn't
  also used for the input, this would only need to handle functions and structs).
- Add a check pass to sanity-check the input (e.g. no duplicate class definitions or method
  arguments, classes and types referenced actually exist, etc).
- Add a type inference pass (so the input doesn't have to be fully annotated with types)
- Add enum/tagged untion types (e.g. Some(x) | None)

Some cleanup I want to do at some point:

- Rip out the `__str__` code on all the syntax classes and replace it with just
  `render_lisp(self.to_lisp())`.
- Rename `scope` and `locals` in the lowering context because those names (especially locals) are
  confusing.
- Create a function to nicely format the output (maybe not a full pretty-print algorithm that
  handles a target line length, just simple logic that breaks common things like if statements into
  multiple lines).
- When lowering uses of class methods on concrete types (e.g. `show 123`), instead of making the
  entire instance dictionary then accessing one method out of that dictionary, it makes more sense
  to hard-code a reference to that particular function instead. Doing this requires making
  stand-alone functions for the instance methods instead of them being lambda functions in the
  dictionary constructor.
