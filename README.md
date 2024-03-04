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
    (length s)))

(instance (Child String)
  (fn child (s)
    (inc (parent s))))

(fn use-child-class (x)
  (child x))
```

Functions can be given explicit types, e.g.


```
(fn use-child-class (x)
  (=> ((Child a)) (Fn a Int))
  (child x))
```

Expressions can also be typed by wrapping them in a `(:: <expression> <type>)` annotation, e.g.

```
(+ (:: foo Int) (:: bar Int))
```


## Status

This has a parser front-end that uses an ml-like lisp-based syntax.

The next steps are:

- [In progress] Add a type inference pass (so the input doesn't have to be fully annotated with types)
- Add enum/tagged untion types (e.g. Some(x) | None)

Things that would be nice to add:

- A bytecode VM
- Add a kinds system to ensure types are correctly constructed
- Compilation to x86?

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
- Add line/column numbers to the syntax objects and to error messages.
- Support more than one error message at once.
