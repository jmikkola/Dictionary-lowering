# LowerDictPy

This is another attempt at writing dictionary-passing. The plan here is to keep everything else as
easy to work with as possible, meaning (a) an ML-like execution model and (b) a very simple
tree-walking interpreter on the backend.

## Installing and running

This depends on Python 3.

`make installdeps` to install the python tools used for development.

`make test` to test, `make check` to run some basic lints.


## Status

The basic lowering logic is there, but it is only called from tests right now. The main executable
doesn't do anything.

This has a parser front-end that uses an ml-like lisp-based syntax.

- [Next] Add a front-end that can read files
- And a treewalking interpreter. This will shake out any major incorrect assumptions in how the
  lowering is done.
- Add a type check pass after the lowering is done to sanity-check the output (assuming this isn't
  also used for the input, this would only need to handle functions and structs).
- Add a check pass to sanity-check the input (e.g. no duplicate class definitions or method
  arguments, classes and types referenced actually exist, etc).
- Add a type inference pass (so the input doesn't have to be fully annotated with types)

Some cleanup I want to do at some point:

- Rip out the `__str__` code on all the syntax classes and replace it with just
  `render_lisp(self.to_lisp())`.
- Rename `scope` and `locals` in the lowering context because those names (escpecially locals) are
  confusing.
- Create a function to nicely format the output (maybe not a full pretty-print algorithm that
  handles a target line length, just simple logic that breaks common things like if statements into
  multiple lines).
