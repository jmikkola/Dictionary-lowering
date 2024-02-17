# module check

from interpreter.program import Program
from interpreter import graph


def check(program: Program):
    ''' This is a pass that should run right after the parser and
    before any type or lowering passes'''

    # TODO:
    # - Check for duplicate:
    #     - struct fields
    #     - lambda arg names
    #     - let binding names
    # - Check expressions:
    #     - Refer to defined variables or functions (which includes class
    #     methods) (requires defining the built-in classes and functions)
    # - Check instances:
    #     - Reference a valid type (requires defining the built-in type names)
    #     - Reference a valid class
    #     - Match methods by name and number of args
    #     - Don't overlap with each other
    # - Check types:
    #     - Check for references to undefined type constructors or variables
    #     - Check for zero-arg function types
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
        self._check_functions(self.program.functions)
        self._check_structs(self.program.structs)
        self._check_classes(self.program.classes)
        self._check_instances(self.program.instances)
        self._check_function_names(
            self.program.functions,
            self.program.classes
        )
        self._check_declaration_names(
            self.program.classes,
            self.program.structs
        )

    def _check_function_names(self, functions, classes):
        function_names = [f.name for f in functions]
        method_names = [m.method_name for c in classes for m in c.methods]
        self._assert_unique(
            function_names + method_names,
            lambda name: CheckFailure(f'Duplicate method name {name}')
        )

    def _check_declaration_names(self, classes, structs):
        class_names = [c.class_name() for c in classes]
        struct_names = [s.name for s in structs]
        self._assert_unique(
            class_names + struct_names,
            lambda name: CheckFailure(f'Duplicate declaration name {name}')
        )

    def _check_functions(self, functions):
        for f in functions:
            self._check_function(f)

    def _check_function(self, function):
        self._assert_unique(
            function.arg_names,
            lambda name: CheckFailure(f'Duplicate argument {name} in function {function.name}')
        )

        self._check_valid_name('function', function.name)

    def _check_structs(self, structs):
        pass

    def _check_classes(self, classes):
        self._assert_unique(
            (c.class_name() for c in classes),
            lambda name: CheckFailure(f'Duplicate classes named {name}')
        )

        for c in classes:
            self._check_class(c)

        class_deps = []
        for c in classes:
            for s in c.supers:
                class_deps.append((c.class_name(), s.name))
        result = graph.topological_order(class_deps)
        if result is False:
            raise CheckFailure(f'Class hierarchy cannot be cyclic')

    def _check_class(self, c):
        self._assert_unique(
            (s.name for s in c.supers),
            lambda name: CheckFailure(f'Duplicate super class {name} for class {c.class_name()}')
        )

        self._assert_unique(
            (m.method_name for m in c.methods),
            lambda name: CheckFailure(f'Duplicate method {name} for class {c.class_name()}')
        )

        for m in c.methods:
            self._check_class_method(c, m)

    def _check_class_method(self, c, m):
        method_type = m.get_type()
        self._check_type(method_type)

        ftvs = method_type.free_type_vars()
        if c.tvar not in ftvs:
            raise CheckFailure(f'Method {m.method_name} on class {c.class_name()} must reference the class type variable')

    def _check_instances(self, instances):
        pass

    def _check_expression(self, expr):
        pass

    def _check_type(self, t):
        pass

    def _check_valid_name(self, name_kind, name):
        first_letter = name[0]
        if not first_letter.isalpha() or not first_letter.islower():
            raise CheckFailure(f'{name_kind} names must start with a lowercase letter, found {name}')

    def _assert_unique(self, names, make_error):
        seen = set()
        for name in names:
            if name in seen:
                raise make_error(name)
            seen.add(name)
