# module inference

from collections import defaultdict
import typing

from interpreter import builtin
from interpreter import graph
from interpreter import parser
from interpreter import syntax
from interpreter import types
from interpreter.program import Program


def infer_types(program: Program) -> Program:
    inference = Inference(program)
    return inference.infer()


class Inference:
    def __init__(self, program: Program):
        self.program = program
        self.var_count = 0
        self.substitution = types.Substitution({})

        self.class_names = set()  # type: typing.Set[str]
        self.supers_for_class = {}  # type: typing.Dict[str, typing.List[types.TClass]]
        self.instances = defaultdict(list)  # type: typing.Dict[str, typing.List[Instance]]

        self.default_types = [types.TConstructor('Int'), types.TConstructor('Float')]

        # Add built-in classes. These aren't exactly Haskell's classes.
        self.add_class('Eq', [])
        self.add_class('Ord', ['Eq'])
        self.add_class('Show', [])
        self.add_class('Read', [])
        self.add_class('Num', ['Eq', 'Show'])
        self.add_class('Integral', ['Num', 'Ord'])

        # Add built-in instances
        self._add_simple_instance('Eq', 'Int')
        self._add_simple_instance('Eq', 'Float')
        self._add_simple_instance('Eq', 'String')
        self._add_simple_instance('Eq', 'Bool')
        # (Eq a) => (Eq (List a))
        self._parse_and_add_instance('(=> ((Eq a)) (Eq (List a)))')

        self._add_simple_instance('Ord', 'Int')
        self._add_simple_instance('Ord', 'Float')
        self._add_simple_instance('Ord', 'String')
        self._add_simple_instance('Ord', 'Bool')

        self._add_simple_instance('Show', 'Int')
        self._add_simple_instance('Show', 'Float')
        self._add_simple_instance('Show', 'String')
        self._add_simple_instance('Show', 'Bool')
        self._parse_and_add_instance('(=> ((Show a)) (Show (List a)))')

        self._add_simple_instance('Num', 'Int')
        self._add_simple_instance('Num', 'Float')

        self._add_simple_instance('Integral', 'Int')

    def add_classes(self, classes):
        for cls in classes:
            name = cls.class_name()
            self.class_names.add(name)
            self.supers_for_class[name] = cls.supers

    def add_class(self, name: str, supers):
        self.class_names.add(name)
        self.supers_for_class[name] = [types.TClass(s) for s in supers]

    def add_instances(self, instances):
        for inst in instances:
            class_name = inst.tclass.name
            self.add_instance(class_name, Instance.from_qual_pred(inst.qual_pred))

    def add_instance(self, class_name, inst):
        if class_name not in self.class_names:
            raise RuntimeError(f'No class definition for {class_name}')
        self.instances[class_name].append(inst)

    def _add_simple_instance(self, class_name, constructor_name):
        tclass = types.TClass(class_name)
        t = types.TConstructor(constructor_name)
        self.add_instance(class_name, Instance([], tclass, t))

    def _parse_and_add_instance(self, text: str):
        sexpr = parser._parse_lists(text)[0]
        qual_pred = parser._parse_qualified_predicate(sexpr)
        self.add_instance(qual_pred.t.tclass.name, Instance.from_qual_pred(qual_pred))

    def infer(self):
        self.add_classes(self.program.classes)
        self.add_instances(self.program.instances)

        # Split bindings into explicitly typed and implicitly typed groups
        explicit_typed, implicit_typed_groups = split_bindings(self.program.functions)

        predicates = []
        assumptions = {}

        # Add the explicit types to the assumptions
        for f in explicit_typed:
            assumptions[f.name] = self.generalize(f.type)

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
        retained_predicates = self.reduce(predicates)

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

    def infer_literal(self, lit: syntax.Literal):
        # Integer literals can be used as any numeric type
        if isinstance(lit, syntax.LInt):
            tvar = self.next_type_var()
            return ([pred("Num", tvar)], tvar)

        # Other types are simpler
        return ([], lit.get_type())

    def infer_expression(self, assumptions, expr: syntax.Expression):
        # TODO: handle user-defined expression types

        predicates, t = self._infer_expression(assumptions, expr)

        expr.t = t

        return (predicates, t)

    def _infer_expression(self, assumptions, expr: syntax.Expression):
        if isinstance(expr, syntax.EVariable):
            scheme = assumptions.get_scheme(expr.name)
            qual = self.instantiate(scheme)
            return (qual.predicates, qual.t)

        elif isinstance(expr, syntax.ELiteral):
            return self.infer_literal(expr.literal)

        elif isinstance(expr, syntax.ECall):
            predicates, fn_type = self.infer_expression(assumptions, expr.f_expr)

            arg_types = []  # type: typing.List[types.Type]
            for arg in expr.arg_exprs:
                ps, arg_t = self.infer_expression(assumptions, arg)
                predicates.extend(ps)
                arg_types.append(arg_t)

            # Create a new type variable to represent the type returned by this
            # expression
            ret_type = self.next_type_var()

            # Build a function type using the known arg types and that
            # placeholder return type
            expected_fn_type = types.make_function_type(arg_types, ret_type)

            # Unify that type with the actual funciton's type. This will fail
            # if the argument types don't work for that function. Assuming this
            # succeeds, it will update the substitution so that it can update
            # the return type to the actual final return type.
            self.unify_types(expected_fn_type, fn_type)

            return (predicates, ret_type)

        elif isinstance(expr, syntax.EConstruct):
            struct_def = self.get_struct_def(expr.struct_name)

            predicates = []
            arg_types = []
            for arg in expr.arg_exprs:
                ps, arg_t = self.infer_expression(assumptions, arg)
                predicates.extend(ps)
                arg_types.append(arg_t)

            struct_type = self.struct_def_to_type(struct_def)
            field_types = struct_def.get_field_types()

            if len(field_types) != len(arg_types):
                raise TypeError(f'Struct {expr.struct_name} has {len(field_types)} fields but constructed with {len(arg_types)}')

            # TODO: this type, plus the types of the field access "functions", could be precomputed
            struct_fn_type = types.make_function_type(field_types, struct_type)

            ret_type = self.next_type_var()
            expected_fn_type = types.make_function_type(arg_types, ret_type)

            self.unify_types(expected_fn_type, struct_fn_type)

            return predicates, ret_type

        elif isinstance(expr, syntax.EPartial):
            raise RuntimeError('Partial application should not exist until lowering')

        elif isinstance(expr, syntax.EAccess):
            predicates, inner_t = self.infer_expression(assumptions, expr.lhs)

            # Require that type inference have already figured out the type
            # well enough to know that it definitively is a struct type and
            # which struct type it is.
            # This will fail if the information seen so far hasn't been enough
            # to determine the struct's type.
            # It might be the case that other code later in the function would
            # clarify what type of struct this is, but that isn't considered
            # here. A more advanced algorithm might try to defer figuring out
            # the kind of struct until the end of the function. This gets
            # complicated when other types (including other struct accesses)
            # depend on the type of this expression.
            struct_name = types.require_type_constructor(inner_t)

            # Look up the definition of the struct by name
            struct_def = self.get_struct_def(struct_name)
            # And look up the field within that struct being accessed
            field_type = struct_def.find_field_type(expr.field)
            if field_type is None:
                raise types.TypeError(f'Struct {struct_name} does not have a field {expr.field}')

            # Create a type like (Pair a b) out of the struct's definition
            struct_type = self.struct_def_to_type(struct_def)
            # Field access is effectively like a function that takes the struct
            # value as an argument and returns the value of a field. This code
            # creates a type for that imaginary function to do the type
            # operations on. This isn't necessary, it's just for convenience.
            # E.g. (Fn (Pair a b) a).
            field_fn_type = types.make_function_type([struct_type], field_type)

            # Convert that type to a scheme, like (Fn (Pair $0 $1) $0)
            scheme = types.Scheme.quantify(
                field_fn_type.free_type_vars(),
                types.Qualified([], field_fn_type)
            )
            # Instantiate that scheme to get a unique type for the type variables,
            # e.g. (Fn (Pair t10 t11) t10)
            fresh_field_fn_type = self.instantiate(scheme)

            # Now, similar to the code for ECall, create a type variable to
            # represent the resulting type, then build a function type out of
            # it.
            ret_type = self.next_type_var()
            expected_field_fn_type = types.make_function_type([inner_t], ret_type)

            # And unify them to find out what ret_type ends up being
            self.unify_types(expected_field_fn_type, fresh_field_fn_type)

            return predicates, ret_type

        elif isinstance(expr, syntax.ELet):
            binding_tvars = {
                binding.name: self.next_type_var()
                for binding in expr.bindings
            }
            temp_assumptions = assumptions.make_child({
                name: types.Scheme.to_scheme(tvar)
                for (name, tvar) in binding_tvars.items()
            })

            predicates = []
            binding_types = {}

            for binding in expr.bindings:
                ps, binding_t = self.infer_expression(temp_assumptions, binding.value)
                predicates.extend(ps)
                binding_types[binding.name] = binding_t
                self.unify_types(binding_t, binding_tvars[binding.name])

            # TODO: Should this handle splitting predicates into
            # deferred/retained and applying the monomorphism restriction?
            # Bindings that are not binding lambda functions should also have
            # the monomorphism restriction applied.
            # (Which doesn't mean the type can't generalize, just that it can't
            # have predicates.)
            # (Keeping predicates on these bindings may require changing some
            # lowering logic)

            # Update the types with the information learned after typing all the bindings
            # (one binding might narrow the type of another)
            binding_types = {
                name: t.apply(self.substitution)
                for (name, t) in binding_types.items()
            }

            # Find what type variables are actually new in these binding's types
            # (vs. those already bound at some outer binding)
            binding_vars = set()
            for t in binding_types.values():
                binding_vars |= t.free_type_vars()
            free_vars = binding_vars - assumptions.free_type_vars()

            # Now that the types of the bindings are known, use that information to
            # generalize them.

            inner_assumptions = assumptions.make_child({
                name: self.quantify(free_vars, t)
                for (name, t) in binding_types.items()
            })

            ps, inner_t = self.infer_expression(inner_assumptions, expr.inner)
            predicates.extend(ps)

            return (predicates, inner_t)

        elif isinstance(expr, syntax.EIf):
            predicates = []

            ps, test_t = self.infer_expression(assumptions, expr.test)
            self.unify_types(test_t, types.TConstructor('Bool'))
            predicates.extend(ps)

            ps, if_t = self.infer_expression(assumptions, expr.if_case)
            predicates.extend(ps)

            ps, else_t = self.infer_expression(assumptions, expr.else_case)
            predicates.extend(ps)

            result_t = self.next_type_var()
            self.unify_types(result_t, if_t)
            self.unify_types(result_t, else_t)

            return (predicates, result_t)

        elif isinstance(expr, syntax.ELambda):
            # Compute the list first to ensure the order is stable
            arg_tvars = [self.next_type_var() for _ in expr.arg_names]
            arg_types = {
                name: types.Scheme.to_scheme(tvar)
                for (name, tvar) in zip(expr.arg_names, arg_tvars)
            }
            body_assumptions = assumptions.make_child(arg_schemes)

            predicates, body_t = self.infer_expression(body_assumptions, expr.body)

            # This type only gets generalized if it is bound to a name
            # in a let binding.
            lambda_t = types.make_function_type(arg_tvars, body_t)
            return predicates, lambda_t

        else:
            raise RuntimeError(f'Unhandled type of expression: {expr}')

    def apply_types_to_functions(self, explicit_typed, implicit_typed_groups):
        pass

    def apply_types_to_instances(self, instances):
        pass

    def split(self, context_tvars, binding_tvars, predicates):
        predicates = self.reduce(predicates)

        context_tvar_set = set(context_tvars)

        deferred = []
        retained = []

        for predicate in predicates:
            pred_tvars = predicate.t.type_variables()
            # check if all type varibles in the predicate are from the context:
            if pred_tvars.issubset(context_tvar_set):
                deferred.append(predicate)
            else:
                retained.append(predicate)

        defaulted = self.defaulted_predicates(context_tvars + binding_tvars, retained)

        retained = [
            p for p in retained
            if p not in defaulted
        ]

        return deferred, retained

    def apply_defaults(self, predicates):
        ambiguities = self.find_ambiguities([], predicates)

        sub = {}
        for a in ambiguities:
            candidate_types = self.candidates(a)
            if len(candidate_types) == 0:
                raise types.TypeError(f'No default type for {a.type_variable}')
            sub[a.type_variable] = candidate_types[0]

        return types.Substitution(sub)

    def defaulted_predicates(self, type_variables, predicates):
        ambiguities = self.find_ambiguities(type_variables, predicates)

        results = []
        for a in ambiguities:
            candidate_types = self.candidates(a)
            if len(candidate_types) == 0:
                raise types.TypeError(f'No default type for {a.type_variable}')
            results.extend(a.predicates)

        return results

    def candidates(self, ambiguity):
        ''' Find concrete types that are a candidate for an ambiguous type. '''
        assert(isinstance(ambiguity, Ambiguity))

        for pred in ambiguity.predicates:
            # Only predicates on type variables are resolvable ambiguities
            if not isinstance(pred.t, types.TVariable):
                return []

        class_names = [p.tclass.name for p in ambiguity.predicates]

        # Only number types are resolvable
        if 'Num' not in class_names:
            return []

        # All classes must be standard classes
        std_classes = builtin.STD_CLASSES
        for c in class_names:
            if c not in std_classes:
                return []

        candidate_types = []
        for t in self.default_types:
            all_entailed = True
            for p in ambiguity.predicates:
                # Does the predicate p hold for this candidate type t?
                # (Which basically means is there an instance of the class for t)
                candidate_predicate = types.Predicate(p.tclass, t)
                if not self.entails([], candidate_predicate):
                    all_entailed = False
                    break
            if all_entailed:
                candidate_types.append(t)

        return candidate_types

    def find_ambiguities(self, type_variables, predicates):
        ''' Find predicates that are ambiguous because they refer to a type variable that's not part of the binding's type.

        E.g. in '(Show a) => b -> String', the predicate (Show a) is ambiguous because 'a' is not part of the type 'b -> String'.

        This can happen when a variable inside a function has an ambiguous type (e.g. we know it's some kind of number, but don't know what kind).
        This can happen with code like:
          fromInteger x > fromInteger y
        (fromInteger x could return any number type such as Float and this would be valid, so it's ambiguous which type to use.)

        The resulting ambiguities are grouped by the type variable they refer to.
        '''
        tv_set = set(type_variables)

        predicates_by_type_variable = defaultdict(list)
        for p in predicates:
            predicates_by_type_variable[p.t.type_variable].append(p)

        return [
            Ambiguity(tv, preds)
            for (tv, preds) in predicates_by_type_variable.items()
            if tv not in tv_set
        ]

    def to_head_normal_form_list(self, predicates):
        ''' Converts a list of predicates to head normal form. '''
        results = []
        for p in predicates:
            results.extend(self.to_head_normal_form(p))
        return results

    def to_head_normal_form(self, predicate):
        ''' Converts a predicate to head normal form. '''
        if self.in_head_normal_form(predicate):
            return [predicate]

        # If the predicate is something like `(Show (List Int))`, try to find an
        # instance of Show for Int. If there's no instance that matches, then context
        # reduction fails. If there is an instance that matches, that instance might
        # give predicates of it's own (in this case, probably `(Show Int)`), so recursively
        # process those.
        predicates = self.by_instances(predicate)
        if predicates is None:
            raise types.TypeError(f'Context reduction fails for {predicate}')

        return self.to_head_normal_form_list(predicates)

    def in_head_normal_form(self, predicate):
        assert(isinstance(predicate, types.Predicate))
        return self.type_in_head_normal_form(predicate.t)

    def type_in_head_normal_form(self, t):
        assert(isinstance(t, types.Type))
        if isinstance(t, types.TVariable):
            return True
        elif isinstance(t, types.TConstructor):
            return False
        elif isinstance(t, types.TApplication):
            return self.type_in_head_normal_form(t.t)
        else:
            raise RuntimeError(f'Unhandled type: {t}')

    def by_instances(self, predicate):
        ''' Find an instances of the class mentioned in the predicate that matches the predicate's type.

        This returns any predicates that instance needs. '''
        instances = self.get_instances(predicate.tclass)

        for instance in instances:
            try:
                substitution = self.match_predicate(instance.get_predicate(), predicate)
                return substitution.apply_to_list(instance.get_predicates())
            except types.TypeError:
                # The instance didn't match, so try the next one
                continue

    def match_predicate(self, instance_predicate, predicate):
        ''' Matches the instance predicate to the given predicate, returning the substitution that makes them equal. '''
        if instance_predicate.tclass != predicate.tclass:
            raise types.TypeError('Class mismatch')

        return types.match(instance_predicate.t, predicate.t)

    def get_predicates_for_superclasses(self, predicate: types.Predicate):
        ''' Returns the current predicate plus the predicates that you also get because their classes are superclasses of the current class. '''
        result = [predicate]

        for superclass in self.supers_for_class[predicate.tclass.name]:
            p = types.Predicate(superclass, predicate.t)
            result.extend(self.get_predicates_for_superclasses(p))

        return result

    def entails(self, given_predicates, p):
        ''' Returns true if the given predicates entail the predicate p. '''
        # Return true if one of the given predicates or its superclasses match p
        for given in given_predicates:
            if p in self.get_predicates_for_superclasses(given):
                return True

        # Return true if there is an instance of the class that matches p
        # _and_ all the predicates for that instance are also entailed.
        instance_preds = self.by_instances(p)
        if instance_preds is None:
            return False

        for inst_p in instance_preds:
            if not self.entails(given_predicates, inst_p):
                return False

        return True

    def simplify(self, predicates):
        ''' Removes any predicate from the list that is entailed by the others. '''
        results = []

        for i in range(len(predicates)):
            p = predicates[i]
            other_predicates = predicates[:i] + predicates[i+1:]
            # Keep p in the output if it is not entailed by the other predicates
            if not self.entails(other_predicates, p):
                results.append(p)

        return results

    def reduce(self, predicates):
        ''' Convert predicates to HNF then simplify them. '''
        predicates = self.to_head_normal_form_list(predicates)
        return self.simplify(predicates)

    def get_instances(self, tclass: types.TClass):
        return self.instances.get(tclass.name, [])

    def next_type_var(self):
        self.var_count += 1
        return types.TVariable.from_varname(f't{self.var_count}')

    def instantiate(self, scheme: types.Scheme):
        fresh_types = [self.next_type_var() for _ in range(scheme.n_vars)]
        return scheme.instantiate(fresh_types)

    def quantify(self, tvars, t: types.Type) -> types.Scheme:
        return types.Scheme.quantify(
            tvars,
            types.Qualified([], t)
        )

    def generalize(self, t: types.Type) -> types.Scheme:
        return types.Scheme.quantify(
            t.free_type_vars(),
            types.Qualified([], t)
        )

    def unify_types(self, t1: types.Type, t2: types.Type):
        sub = types.most_general_unifier(
            t1.apply(self.substitution),
            t2.apply(self.substitution)
        )
        self.substitution = self.substitution.compose(sub)

    def get_struct_def(self, struct_name: str):
        for struct in self.program.structs:
            if struct.name == struct_name:
                return struct
        raise types.TypeError('{struct_name} is not a struct')

    def struct_def_to_type(self, struct_def: syntax.StructDef):
        constructor = types.TConstructor(struct_def.name)

        if not struct_def.type_vars:
            return constructor

        tvars = [types.TVariable(tv) for tv in struct_def.type_vars]
        return types.TApplication(constructor, tvars)


class Ambiguity:
    def __init__(self, type_variable, predicates):
        self.type_variable = type_variable
        self.predicates = predicates

    def __eq__(self, o):
        return (
            isinstance(o, Ambiguity) and
            self.type_variable == o.type_variable and
            self.predicates == o.predicates
        )

    def __repr__(self):
        return f'Ambiguity({self.type_variable}, {self.predicates})'

    def __str__(self):
        return repr(self)


class Instance:
    def __init__(self, predicates, tclass, t):
        self.predicates = predicates
        self.tclass = tclass
        self.t = t

    @classmethod
    def from_qual_pred(cls, qual_pred):
        return cls(qual_pred.predicates, qual_pred.t.tclass, qual_pred.t.t)

    def get_predicates(self):
        ''' Return the predicates that are required to use this instance. '''
        return self.predicates

    def get_predicate(self):
        ''' Return the class implemented and the type implemented for as a predicate. '''
        return types.Predicate(self.tclass, self.t)


class Assumptions:
    def __init__(self, assumptions=None, parent=None):
        self.assumptions = assumptions or {}
        self.parent = parent

    def define(self, name, scheme: types.Scheme):
        self.assumptions[name] = scheme

    def get_scheme(self, name) -> types.Scheme:
        if name in self.assumptions:
            return self.assumptions[name]
        elif self.parent:
            return self.parent.get_scheme(name)
        else:
            raise RuntimeError(f'No type for {name}')

    def make_child(self, assumptions=None):
        return Assumptions(assumptions=assumptions, parent=self)

    def free_type_vars(self):
        tvars = set()
        for scheme in self.assumptions.values():
            tvars |= scheme.free_type_vars()
        if self.parent is not None:
            tvars |= self.parent.free_type_vars()
        return tvars


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


def pred(class_name, t):
    return types.Predicate(types.TClass(class_name), t)
