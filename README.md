# LowerDictPy

This is another attempt at writing dictionary-passing. The plan here is to keep everything else as
easy to work with as possible, meaning (a) an ML-like execution model and (b) a very simple
tree-walking interpreter on the backend.


## Status

The basic lowering logic has been ported from the Haskell project.

This has a parser front-end that uses an ml-like lisp-based syntax.

- [in progress] The immediate next step is to use the parser to build unit tests of the lowering logic.

- And a treewalking interpreter
- Idea: Add a type check pass after the lowering is done to sanity-check the output?
- Idea: Add a way to convert syntax/types back to s-expressions for the sake of nice output. This
  could also be part of the parser test - the output should be the same as the input s-expressions.


Some cleanup I want to do at some point:

- Rip out the `__str__` code on all the syntax classes and replace it with just
  `render_lisp(self.to_lisp())`.
- Rename `scope` and `locals` in the lowering context because those names (escpecially locals) are
  confusing.
- Create a function to nicely format the output (maybe not a full pretty-print algorithm that
  handles a target line length, just simple logic that breaks common things like if statements into
  multiple lines).
