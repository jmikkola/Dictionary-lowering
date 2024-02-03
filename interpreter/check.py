# module check

from interpreter.program import Program


def check(program: Program):
    ''' This is a pass that should run right after the parser and
    before any type or lowering passes'''

    # TODO:
    # - check for duplicate top-level declarations (classes, functions)
    # - check for overlapping instances
    # - check for duplicate arg definitions (in functions and lambdas)
    # - check for duplicate variable definitions (in let bindings)
    # - check for references to undefined classes, types, functions, or variables
    checker = Checker(program)
    checker.check()

    # Checker doesn't actually change the code in any way except to note which
    # pass it went through:
    return Program(
        from_stage='check',
        functions=program.functions,
        structs=program.structs,
        classes=program.classes,
        instances=program.instances,
    )


class CheckFailure(RuntimeError):
    pass


class Checker:
    def __init__(self, program: Program):
        self.program = program

    def check(self):
        self._check_function_names(
            self.program.functions,
            self.program.classes
        )
        self._check_functions(self.program.functions)
        self._check_structs(self.program.structs)
        self._check_classes(self.program.classes)
        self._check_instances(self.program.instances)

    def _check_function_names(self, functions, classes):
        pass

    def _check_functions(self, functions):
        pass

    def _check_structs(self, structs):
        pass

    def _check_classes(self, classes):
        self._assert_unique(
            (c.class_name() for c in classes),
            lambda name: CheckFailure(f'Duplicate classes named {name}')
        )

        for c in classes:
            self._check_class(c)

        # TODO: assert class hierarchy is acyclic

    def _check_class(self, c):
        self._assert_unique(
            (s.name for s in c.supers),
            lambda name: CheckFailure(f'Duplicate super class {name} for class {c.class_name()}')
        )

        self._assert_unique(
            (m.method_name for m in c.methods),
            lambda name: CheckFailure(f'Duplicate method {name} for class {c.class_name()}')
        )

        # TODO: assert that the methods types contain the class's type variable


    def _check_instances(self, instances):
        pass

    def _check_expression(self, expr):
        pass

    def _check_type(self, t):
        pass

    def _assert_unique(self, names, make_error):
        seen = set()
        for name in names:
            if name in seen:
                raise make_error(name)
            seen.add(name)
