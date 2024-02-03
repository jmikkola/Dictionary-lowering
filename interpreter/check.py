# module check

def check(program):
    ''' This is a pass that should run right after the parser and
    before any type or lowering passes'''

    # TODO:
    # - check for duplicate top-level declarations (classes, functions)
    # - check for overlapping instances
    # - check for duplicate arg definitions (in functions and lambdas)
    # - check for duplicate variable definitions (in let bindings)
    # - check for references to undefined classes, types, functions, or variables
    return program
