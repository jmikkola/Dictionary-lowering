# LowerDictPy

This is another attempt at writing dictionary-passing. The plan here is to keep everything else as
easy to work with as possible, meaning (a) an ML-like execution model and (b) a very simple
tree-walking interpreter on the backend.


## Status

The basic lowering logic has been ported from the Haskell project.

This has a parser front-end that uses an ml-like lisp-based syntax.

- The immediate next step is to use the parser to build unit tests of the lowering logic.
- The next milestone is to have it read in (typed) code with classes and instances and output
  lowered code (print it out), making it possible to visually inspect that this works and is sane.
    - It might be useful to add a typecheck pass that can check that the user-provided types are
      correct. This would also be useful for sanity-checking the output of the lowering pass.

- And a treewalking interpreter
- Idea: Add a type check pass after the lowering is done to sanity-check the output?
- Idea: Add a way to convert syntax/types back to s-expressions for the sake of nice output. This
  could also be part of the parser test - the output should be the same as the input s-expressions.


Some cleanup I want to do at some point:

- Rip out the `__str__` code on all the syntax classes and replace it with just
  `render_lisp(self.to_lisp())`.
