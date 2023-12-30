# LowerDictPy

This is another attempt at writing dictionary-passing. The plan here is to keep everything else as
easy to work with as possible, meaning (a) an ML-like execution model and (b) a very simple
tree-walking interpreter on the backend.


## Status

Just a skeleton project so far

- I'm porting stuff from dicionarypassing.hs
    - The next thing to port is the lowering logic
- Then, add a simple front-end (ml-ish syntax?)
- The next milestone is to have it read in (typed) code with classes and instances and output
  lowered code (print it out), making it possible to visually inspect that this works and is sane.

- And a treewalking interpreter
- Idea: Add a type check pass after the lowering is done to sanity-check the output?
