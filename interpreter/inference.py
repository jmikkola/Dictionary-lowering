# module inference

from interpreter import graph
from interpreter import syntax
from interpreter import types
from interpreter.program import Program


def infer_types(program: Program) -> Program:
    inference = Inference(program)
    return inference.infer()


class Inference:
    def __init__(self, program):
        self.program = program
        self.var_count = 0
        self.substitution = types.Substitution({})

    def infer(self):
        # TODO: Build class environment
        # TODO: Add builtins to the environment

        # Split bindings into explicitly typed and implicitly typed groups
        explicit_typed, implicit_typed_groups = split_bindings(self.program.functions)

        predicates = []
        assumptions = {}

        # Add the explicit types to the assumptions
        for f in explicit_typed:
            assumptions[f.name] = f.type

        # Infer the types of the implicitly typed functions
        for group in implicit_typed_groups:
            preds, aspts = self.infer_group(group, Assumptions(assumptions))
            predicates.extend(preds)
            assumptions.update(aspts)

        # Check that the types of explicitly typed functions are valid
        for f in explicit_typed:
            preds = self.infer_explicit(f, Assumptions(assumptions))
            predicates.extend(preds)

        # Finally, check the types of types in instances
        instances = self.infer_instances(self.program.instances, Assumptions(assumptions))

        # Update the predicates with the current substitution
        predicates = self.substitution.apply_to_list(predicates)

        # Get rid of any predicates that obviously hold (e.g. (Show Int))
        retained_predicates = self.apply_context_reduction(predicates)

        # Of the predicates that remain, pick default types where possible
        # (e.g. replace (=> (Num a) a) with Int).
        defauling_substitution = self.apply_defaults(retained_predicates)
        self.substitution = self.substitution.compose(defauling_substitution)

        # Now that the final types are known, go back and update the type information on the functions
        typed_functions = self.apply_types_to_functions(explicit_typed, implicit_typed_groups)
        typed_instances = self.apply_types_to_instances(instances)

        # Return the program with types inferred
        return Program(
            'inference',
            functions=typed_functions,
            structs=self.program.structs,
            classes=self.program.classes,
            instances=typed_instances,
        )

    def infer_group(self, functions, assumptions):
        ''' Infers the types of the given group of implicitly-typed functions. '''
        # TODO
        pass

    def infer_explicit(self, function, assumptions):
        ''' Infers the type of the given explicitly-typed function. '''
        # TODO
        pass

    def infer_instances(self, instances, assumptions):
        ''' Infers the types of the given instances. '''
        # TODO
        pass

    def apply_context_reduction(self, predicates):
        pass

    def apply_defaults(self, predicates):
        pass

    def apply_types_to_functions(self, explicit_typed, implicit_typed_groups):
        pass

    def apply_types_to_instances(self, instances):
        pass


class Assumptions:
    def __init__(self, assumptions=None, parent=None):
        self.assumptions = assumptions or {}
        self.parent = parent

    def define(self, name, type):
        self.assumptions[name] = type

    def get_type(self, name):
        if name in self.assumptions:
            return self.assumptions[name]
        elif self.parent:
            return self.parent.get_type(name)
        else:
            raise RuntimeError(f'No type for {name}')


def split_bindings(functions):
    ''' Splits bindings into those that are explicitly typed (the first return value) and those that are implicitly typed (the second return value).

    The implicitly-typed functions are returned as a topologically sorted list of groups of
    functions. Each group is a list of functions that are mutually recursive.
    '''
    explicit_typed = [f for f in functions if f.t is not None]

    implicit_typed = [f for f in functions if f.t is None]
    implicit_typed_groups = group_call_graph(implicit_typed)

    return explicit_typed, implicit_typed_groups


def group_call_graph(functions):
    ''' Groups functions by their call graph into groups of mutually-recurisve functions.'''
    function_names = {f.name for f in functions}

    # The call graph contains the names of the functions
    call_graph = make_call_graph(functions, function_names)
    components = graph.strongly_connected_components(call_graph)

    # Group the actual functions by component
    functions_by_name = {f.name: f for f in functions}
    return [
        [functions_by_name[name] for name in component]
        for component in components
    ]


def make_call_graph(functions, function_names):
    ''' Returns a dictionary of function names to the names of the functions they call.'''
    # Take the intersection of the result of get_called_functions with function_names to
    # exclude names that aren't being considered (e.g. builtins and explicitly-typed functions)
    return {
        f.name: get_called_functions(f) & function_names
        for f in functions
    }


def get_called_functions(function: syntax.DFunction):
    ''' Returns the names of the functions referenced by the given function.'''
    scope = Scope()
    scope.define_all(function.arg_names)
    return get_unbound_variables(function.body, scope)


def get_unbound_variables(expr, scope):
    ''' Returns the set of unbound variables in the given expression. '''
    if isinstance(expr, syntax.EVariable):
        if not scope.is_defined(expr.name):
            return set([expr.name])
        else:
            return set()

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


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.names = set()

    def define_all(self, names):
        self.names.update(names)

    def make_inner(self, names):
        inner = Scope(parent=self)
        inner.define_all(names)
        return inner

    def is_defined(self, name):
        return name in self.names or (self.parent and self.parent.is_defined(name))