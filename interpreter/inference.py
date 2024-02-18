# module inference

from interpreter import syntax
from interpreter.program import Program


def infer_types(program: Program) -> Program:
    # Split bindings into explicitly typed and implicitly typed groups
    explicit_typed, implicit_typed_groups = split_bindings(program.functions)
    # TODO: infer types for implicit_typed_groups
    # TODO: check types for explicit_typed

    # Return the program with types inferred
    return Program(
        'inference',
        # TODO: Replace with typed functions
        functions=program.functions,
        structs=program.structs,
        classes=program.classes,
        instances=program.instances,
    )


def split_bindings(functions):
    ''' Splits bindings into those that are explicitly typed (the first return value) and those that are implicitly typed (the second return value).

    The implicitly-typed functions are returned as a topologically sorted list of groups of
    functions. Each group is a list of functions that are mutually recursive.
    '''
    all_function_names = {f.name for f in functions}

    explicit_typed = [
        f for f in functions
        if f.type is not None
    ]

    implicit_typed = [
        f for f in functions
        if f.type is None
    ]
    implicit_typed_groups = group_call_graph(implicit_typed, all_function_names)

    return explicit_typed, implicit_typed_groups


def group_call_graph(functions, all_function_names):
    ''' Groups functions by their call graph into groups of mutually-recurisve functions.'''

    call_graph = make_call_graph(functions, all_function_names)
    components = strongly_connected_components(call_graph)
    return components[::-1]


def make_call_graph(functions, all_function_names):
    ''' Returns a dictionary of function names to the names of the functions they call.'''
    # Take the intersection of the result of get_called_functions with all_function_names to
    # exclude names that aren't user-defined functions (e.g. builtins).
    return {
        f.name: get_called_functions(f) & all_function_names
        for f in functions
    }


def get_called_functions(function: syntax.DFunction):
    ''' Returns the names of the functions called by the given function.'''
    scope = Scope()
    scope.define_all(function.arg_names)
    return get_unbound_variables(function.body, scope)


def get_unbound_variables(expr, scope):
    if isinstance(expr, syntax.EVariable):
        if not scope.is_defined(expr.name):
            return set([expr.name])

    elif isinstance(expr, syntax.ELiteral):
        return set()

    elif isinstance(expr, syntax.ECall):
        unbound = get_unbound_variables(expr.f_expr, scope)
        for a in expr.arg_exprs:
            unbound |= get_unbound_variables(a, scope)
        return unbound

    elif isinstance(expr, syntax.EConstruct):
        unbound = set()
        for a in expr.arg_exprs:
            unbound |= get_unbound_variables(a, scope)
        return unbound

    elif isinstance(expr, syntax.EPartial):
        unbound = get_unbound_variables(expr.f_expr, scope)
        for a in expr.arg_exprs:
            unbound |= get_unbound_variables(a, scope)
        return unbound

    elif isinstance(expr, syntax.EAccess):
        return get_unbound_variables(expr.lhs, scope)

    elif isinstance(expr, syntax.ELet):
        names = [binding.name for binding in expr.bindings]
        inner_scope = scope.make_inner(names)
        unbound = set()
        for binding in expr.bindings:
            unbound |= get_unbound_variables(binding.value, inner_scope)
        unbound |= get_unbound_variables(expr.inner, inner_scope)
        return unbound

    elif isinstance(expr, syntax.EIf):
        unbound = get_unbound_variables(expr.test, scope)
        unbound |= get_unbound_variables(expr.if_case, scope)
        unbound |= get_unbound_variables(expr.else_case, scope)
        return unbound

    elif isinstance(expr, syntax.ELambda):
        inner_scope = scope.make_inner(expr.arg_names)
        return get_unbound_variables(expr.body, inner_scope)

    else:
        raise RuntimeError(f'Unhandled expression: {expr}')


def strongly_connected_components(graph):
    ''' Returns a list of strongly connected components in the graph.'''
    pass


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.names = set()

    def define(self, name):
        self.names.add(name)

    def define_all(self, names):
        self.names.update(names)

    def make_inner(self, names):
        inner = Scope(parent=self)
        inner.define_all(names)
        return inner

    def is_defined(self, name):
        return name in self.names or (self.parent and self.parent.is_defined(name))