# LowerDictPy

This is another attempt at writing dictionary-passing. The plan here is to keep everything else as
easy to work with as possible, meaning (a) an ML-like execution model and (b) a very simple
tree-walking interpreter on the backend.


## Status

The basic lowering logic has been ported from the Haskell project.

- The next step is to add a simple front-end (ml-ish syntax?)
- Use that to build unit tests
- The next milestone is to have it read in (typed) code with classes and instances and output
  lowered code (print it out), making it possible to visually inspect that this works and is sane.

- And a treewalking interpreter
- Idea: Add a type check pass after the lowering is done to sanity-check the output?
