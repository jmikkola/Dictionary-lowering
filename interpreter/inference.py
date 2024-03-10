# module inference

from collections import defaultdict
import typing
import functools

from interpreter import builtin
from interpreter import graph
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

        self.class_definitions = {}  # type: typing.Dict[str, syntax.ClassDef]
        self.instances = defaultdict(list)  # type: typing.Dict[str, typing.List[Instance]]
        self.struct_constructor_types = {}  # type: typing.Dict[str, types.Scheme]
        self.struct_field_fn_types = defaultdict(dict)  # type: typing.Dict[str, typing.Dict[str, types.Scheme]]
        self.builtin_assumptions = Assumptions()

        self.default_types = [types.TConstructor('Int'), types.TConstructor('Float')]

        self.add_builtins()

        self.add_classes(self.program.classes)
        self.add_instances(self.program.instances)
        self.add_struct_types(self.program.structs)

    def add_builtins(self):
        # Add built-in classes. These aren't exactly Haskell's classes.
        self.add_classes(builtin.get_classes())
        self.add_instances(builtin.get_instances())

        for name, qual_type in builtin.get_function_types().items():
            scheme = types.Scheme.quantify(qual_type.free_type_vars(), qual_type)
            self.builtin_assumptions.define(name, scheme)

    def add_classes(self, classes):
        for cls in classes:
            self.class_definitions[cls.class_name()] = cls

    def add_instances(self, instances):
        for inst in instances:
            class_name = inst.get_class().name
            self.add_instance(class_name, Instance.from_qual_pred(inst.qual_pred))

    def add_instance(self, class_name, inst):
        if class_name not in self.class_definitions:
            raise RuntimeError(f'No class definition for {class_name}')
        self.instances[class_name].append(inst)

    def add_struct_types(self, structs):
        for struct in structs:
            self.add_struct_type(struct)

    def add_struct_type(self, struct_def):
        assert(isinstance(struct_def, syntax.StructDef))

        struct_type = self.struct_def_to_type(struct_def)
        field_types = struct_def.get_field_types()

        struct_fn_type = types.make_function_type(field_types, struct_type)
        scheme = types.Scheme.quantify(
            struct_fn_type.free_type_vars(),
            types.Qualified([], struct_fn_type)
        )

        self.struct_constructor_types[struct_def.name] = scheme

        for (field_name, field_type) in struct_def.fields:
            self.add_struct_field(struct_def.name, struct_type, field_name, field_type)

    def add_struct_field(self, struct_name, struct_type, field_name, field_type):
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

        self.struct_field_fn_types[struct_name][field_name] = scheme

    def infer(self):
        # Split bindings into explicitly typed and implicitly typed groups
        explicit_typed, implicit_typed_groups = split_bindings(self.program.functions)

        predicates = []
        assumptions = self.builtin_assumptions.make_child(
            self.get_assumptions_for_functions(explicit_typed)
        )

        # Infer the types of the implicitly typed functions
        for group in implicit_typed_groups:
            preds, new_assumptions = self.infer_implicit_group(assumptions, group)

            predicates.extend(preds)
            for (name, t) in new_assumptions.items():
                assumptions.define(name, t)

        # Check that the types of explicitly typed functions are valid
        for f in explicit_typed:
            scheme = assumptions.get_scheme(f.name)
            preds = self.infer_explicit(assumptions, f, scheme)
            predicates.extend(preds)

        # Finally, check the types of types in instances
        instances = self.infer_instances(assumptions, self.program.instances)

        # Update the predicates with the current substitution
        predicates = self.substitution.apply_to_list(predicates)

        # Get rid of any predicates that obviously hold (e.g. (Show Int))
        retained_predicates = self.reduce(predicates)

        # Of the predicates that remain, pick default types where possible
        # (e.g. replace (=> (Num a) a) with Int).
        defauling_substitution = self.apply_defaults(retained_predicates)
        self.substitution = self.substitution.compose(defauling_substitution)

        typed_functions = explicit_typed + [f for group in implicit_typed_groups for f in group]

        # Return the program with types inferred
        return Program(
            'inference',
            functions=typed_functions,
            structs=self.program.structs,
            classes=self.program.classes,
            instances=instances,
        )

    def get_assumptions_for_functions(self, explicit_typed):
        assumptions = {}

        # Add the types of the explicitly-typed functions to the assumptions
        for f in explicit_typed:
            qual_type = f.t
            assumptions[f.name] = types.Scheme.quantify(
                qual_type.free_type_vars(),
                qual_type
            )

        # Class methods are another source of functions with known types
        for cls in self.class_definitions.values():
            class_predicate = cls.get_class_predicate()
            for method in cls.methods:
                method_qual = types.Qualified(
                    [class_predicate] + method.qual_type.predicates,
                    method.qual_type.t
                )
                method_scheme = types.Scheme.quantify(
                    method_qual.free_type_vars(),
                    method_qual
                )
                assumptions[method.method_name] = method_scheme

        return assumptions

    def infer_implicit_group(self, global_assumptions, functions):
        ''' Infers the types of the given group of implicitly-typed functions. '''
        assert(isinstance(global_assumptions, Assumptions))

        # Create a type variable to represent the type of each function
        tvars = {
            f.name: self.next_type_var()
            for f in functions
        }

        # Create a new set of assumptions in which all these functions are defined
        assumptions = global_assumptions.make_child({
            name: types.Scheme.to_scheme(tvar)
            for (name, tvar) in tvars.items()
        })

        predicates = []
        all_expressions = []

        for function in functions:
            assert(isinstance(function, syntax.DFunction))
            expressions = []
            preds, t = self.infer_function(assumptions, function, expressions)
            self.unify_types(t, tvars[function.name])

            predicates.extend(preds)
            all_expressions.extend(expressions)

        predicates = self.substitution.apply_to_list(predicates)
        function_types = {
            name: t.apply(self.substitution)
            for (name, t) in tvars.items()
        }

        updated_assumptions = global_assumptions.apply(self.substitution)
        context_tvars = updated_assumptions.free_type_vars()

        binding_tvars = functools.reduce(
            lambda vars1, vars2: vars1 & vars2,
            (t.free_type_vars() for t in function_types.values())
        )

        deferred, retained = self.split(context_tvars, binding_tvars, predicates)

        # Since all top-level bindings in this language are functions,
        # the monomorphism restriction never applies here.

        # Sort the predicates in a stable order to make tests reliable
        retained = sorted(retained, key=lambda p: (p.tclass.name, str(p.t)))
        function_schemes = {
            name: types.Scheme.quantify(binding_tvars, types.Qualified(retained, t))
            for (name, t) in function_types.items()
        }

        # Save the resulting type back to the functions
        for function in functions:
            t = function_types[function.name]
            function.t = types.Qualified(retained, t)

        self.apply_sub_to_expressions(all_expressions)

        return deferred, function_schemes

    def infer_explicit(self, assumptions, function, scheme):
        ''' Infers the type of the given explicitly-typed function. '''
        assert(isinstance(assumptions, Assumptions))
        assert(isinstance(function, syntax.DFunction))

        # The scheme for function should have been already written to the assumptions
        function_type = self.instantiate(scheme)

        # Inferring the body of the function adds the expressions it sees to this list
        expressions = []

        predicates, inferred_type = self.infer_function(
            assumptions,
            function,
            expressions,
            given_type=function_type.t
        )
        self.unify_types(inferred_type, function_type.t)

        function_type = function_type.apply(self.substitution)

        # Find the type variables that are used only in the function's type
        binding_tvars = assumptions.apply(self.substitution).free_type_vars()
        function_tvars = function_type.free_type_vars() - binding_tvars

        # Create a new scheme from what inference learned about the function's type
        updated_scheme = types.Scheme.quantify(function_tvars, function_type)

        # This would happen if any of the type variables in the user-defined type
        # got replaced with a concrete type.
        if updated_scheme != scheme:
            raise types.TypeError(
                f'Type signature too general for {function.name},' +
                f' expected scheme {scheme} actual scheme {updated_scheme}'
            )

        predicates = self.substitution.apply_to_list(predicates)

        # Find predicates that aren't entailed by the predicates in the user-defined type
        new_predicates = [
            p for p in predicates
            if not self.entails(function_type.predicates, p)
        ]

        deferred, retained = self.split(binding_tvars, function_tvars, new_predicates)
        if retained:
            raise types.TypeError(f'The signature for {function.name} is missing predicates {retained}')

        # Apply the substitution again because it may have been updated by
        # applying default types in the call to .split()
        function.t = function_type.apply(self.substitution)
        self.apply_sub_to_expressions(expressions)

        return deferred

    def infer_function(self, assumptions, function, expressions, given_type=None):
        if given_type is not None:
            assert(isinstance(given_type, types.Type))
            # For functions with explicit types, update the types of the
            # arguments. This is mainly useful so that accessing fields in
            # structs get the type information it needs.
            arg_types = {
                arg: t
                for (arg, t) in zip(function.arg_names, types.get_arg_types(given_type))
            }
        else:
            arg_types = {
                arg: self.next_type_var()
                for arg in function.arg_names
            }


        function_assumptions = assumptions.make_child({
            arg: types.Scheme.to_scheme(tvar)
            for (arg, tvar) in arg_types.items()
        })

        predicates, body_type = self.infer_expression(function_assumptions, function.body, expressions)
        function_type = types.make_function_type(
            [arg_types[arg] for arg in function.arg_names],
            body_type
        )

        # special rule for main
        if function.name == 'main':
            # allow any return value, but require no args
            # DFunction
            if function.arg_names:
                raise types.TypeError(f'function main cannot have args, has {function.arg_names}')

        return predicates, function_type

    def infer_instances(self, assumptions, instances):
        ''' Infers the types of the given instances. '''
        assert(isinstance(assumptions, Assumptions))

        for instance in instances:
            assert(isinstance(instance, syntax.InstanceDef))

            # Find the associated class
            class_def = self.class_definitions.get(instance.get_class().name)
            if class_def is None:
                raise types.TypeError(f'Cannot find definition of class {instance.get_class().name}')

            # Get any prediates on the instance,
            # e.g. if the instance is for (=> ((Show a)) (Show (List a))),
            # then this would be [(Show a)].
            inst_preds = instance.get_predicates()

            for method_impl in instance.method_impls:
                method_def = class_def.get_method(method_impl.name)

                method_qual = types.Qualified(
                    inst_preds + method_def.qual_type.predicates,
                    method_def.qual_type.t
                )

                instance_type_substitution = types.Substitution.singleton(
                    class_def.tvar,
                    instance.get_type()
                )

                method_qual = method_qual.apply(instance_type_substitution)

                method_scheme = types.Scheme.quantify(
                    method_qual.free_type_vars(),
                    method_qual
                )

                # Unlike explicitly-typed bindings, instance method types
                # aren't added to the assumptions at this point because the
                # assumption for the method already exists (due to processing
                # the class). Adding it here would set it to a more specific
                # type, hiding the more general type coming from the class.

                self.infer_explicit(assumptions, method_impl, method_scheme)

        return instances

    def apply_sub_to_expressions(self, expressions):
        ''' Modifies the expressions in-place, applying the given substitution to their types '''
        for expr in expressions:
            assert(isinstance(expr, syntax.Expression) or isinstance(expr, syntax.Binding))
            assert(expr.t is not None)
            expr.t = expr.t.apply(self.substitution)

    def infer_literal(self, lit: syntax.Literal):
        # Integer literals can be used as any numeric type
        if isinstance(lit, syntax.LInt):
            tvar = self.next_type_var()
            return ([pred("Num", tvar)], tvar)

        # Other types are simpler
        return ([], lit.get_type())

    def infer_expression(self, assumptions, expr: syntax.Expression, expressions):
        user_defined_type = expr.t

        predicates, t = self._infer_expression(assumptions, expr, expressions)

        if user_defined_type is not None:
            # Allow the user-defined type to be narrower but not wider than the
            # resulting type
            sub = types.match(t, user_defined_type)
            self.substitution = self.substitution.compose(sub)
            t = t.apply(self.substitution)

        expr.t = t
        expressions.append(expr)

        return (predicates, t)

    def _infer_expression(self, assumptions, expr: syntax.Expression, expressions):
        assert(isinstance(assumptions, Assumptions))

        if isinstance(expr, syntax.EVariable):
            scheme = assumptions.get_scheme(expr.name)
            qual = self.instantiate(scheme)
            return (qual.predicates, qual.t)

        elif isinstance(expr, syntax.ELiteral):
            return self.infer_literal(expr.literal)

        elif isinstance(expr, syntax.ECall):
            predicates, fn_type = self.infer_expression(assumptions, expr.f_expr, expressions)

            arg_types = []  # type: typing.List[types.Type]
            for arg in expr.arg_exprs:
                ps, arg_t = self.infer_expression(assumptions, arg, expressions)
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
            predicates = []
            arg_types = []
            for arg in expr.arg_exprs:
                ps, arg_t = self.infer_expression(assumptions, arg, expressions)
                predicates.extend(ps)
                arg_types.append(arg_t)

            scheme = self.struct_constructor_types[expr.struct_name]
            n_args = types.num_args(scheme.qualified.t)

            if n_args != len(arg_types):
                raise types.TypeError(f'Struct {expr.struct_name} has {n_args} fields but constructed with {len(arg_types)}')

            # Instantiate that scheme to get a unique type for the type variables,
            # e.g. (Fn t1 t2 (Pair t1 t2))
            fresh_struct_fn_type = self.instantiate(scheme).t

            ret_type = self.next_type_var()
            expected_fn_type = types.make_function_type(arg_types, ret_type)

            self.unify_types(expected_fn_type, fresh_struct_fn_type)

            return predicates, ret_type

        elif isinstance(expr, syntax.EPartial):
            raise RuntimeError('Partial application should not exist until lowering')

        elif isinstance(expr, syntax.EAccess):
            predicates, inner_t = self.infer_expression(assumptions, expr.lhs, expressions)
            inner_t = inner_t.apply(self.substitution)

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
            field_name = expr.field

            field_fn_types = self.struct_field_fn_types[struct_name]

            # And look up the field within that struct being accessed
            if field_name not in field_fn_types:
                raise types.TypeError(f'Struct {struct_name} does not have a field {field_name}')

            scheme = field_fn_types[field_name]
            # Instantiate that scheme to get a unique type for the type variables,
            # e.g. (Fn (Pair t10 t11) t10)
            fresh_field_fn_type = self.instantiate(scheme).t

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

            binding_predicates = []
            binding_types = {}

            for binding in expr.bindings:
                ps, binding_t = self.infer_expression(temp_assumptions, binding.value, expressions)
                binding_predicates.extend(ps)
                binding_types[binding.name] = binding_t
                self.unify_types(binding_t, binding_tvars[binding.name])

            # Update the types with the information learned after typing all the bindings
            # (one binding might narrow the type of another)
            binding_types = {
                name: t.apply(self.substitution)
                for (name, t) in binding_types.items()
            }

            binding_predicates = self.substitution.apply_to_list(binding_predicates)

            # Apply the substitution to the context because it may have
            # narrowed the type (or changed type variables) in the type of
            # other variables in the function (e.g. arguments).
            context_tvars = assumptions.apply(self.substitution).free_type_vars()

            # Find what type variables are in all the binding's types
            binding_tvars = functools.reduce(
                lambda vars1, vars2: vars1 & vars2,  # type: ignore
                (t.free_type_vars() for t in binding_types.values())
            )

            deferred, retained = self.split(context_tvars, binding_tvars, binding_predicates)

            # Now that the types of the bindings are known, use that information to
            # generalize them.
            inner_assumptions = assumptions.make_child({
                name: types.Scheme.quantify(
                    binding_tvars,
                    types.Qualified(retained, t)
                )
                for (name, t) in binding_types.items()
            })

            for binding in expr.bindings:
                binding_type = binding_types[binding.name]
                binding_qual = types.Qualified(retained, binding_type)
                binding.t = binding_qual
                expressions.append(binding)

            preds, inner_t = self.infer_expression(inner_assumptions, expr.inner, expressions)

            return (preds + deferred, inner_t)

        elif isinstance(expr, syntax.EIf):
            predicates = []

            ps, test_t = self.infer_expression(assumptions, expr.test, expressions)
            self.unify_types(test_t, types.TConstructor('Bool'))
            predicates.extend(ps)

            ps, if_t = self.infer_expression(assumptions, expr.if_case, expressions)
            predicates.extend(ps)

            ps, else_t = self.infer_expression(assumptions, expr.else_case, expressions)
            predicates.extend(ps)

            result_t = self.next_type_var()
            self.unify_types(result_t, if_t)
            self.unify_types(result_t, else_t)

            return (predicates, result_t)

        elif isinstance(expr, syntax.ELambda):
            # Compute the list first to ensure the order is stable
            arg_tvars = [self.next_type_var() for _ in expr.arg_names]
            arg_schemes = {
                name: types.Scheme.to_scheme(tvar)
                for (name, tvar) in zip(expr.arg_names, arg_tvars)
            }
            body_assumptions = assumptions.make_child(arg_schemes)

            predicates, body_t = self.infer_expression(body_assumptions, expr.body, expressions)

            # This type only gets generalized if it is bound to a name
            # in a let binding.
            lambda_t = types.make_function_type(arg_tvars, body_t)
            return predicates, lambda_t

        else:
            raise RuntimeError(f'Unhandled type of expression: {expr}')

    def split(self, context_tvars, binding_tvars, predicates):
        predicates = self.reduce(predicates)

        deferred = []
        retained = []

        for predicate in predicates:
            assert(isinstance(predicate, types.Predicate))
            pred_tvars = predicate.t.free_type_vars()
            # check if all type varibles in the predicate are from the context:
            if pred_tvars.issubset(context_tvars):
                deferred.append(predicate)
            else:
                retained.append(predicate)

        defaulted = self.defaulted_predicates(context_tvars | binding_tvars, retained)

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
                raise types.TypeError(f'No default type for {a.type_variable} {predicates}')
            sub[a.type_variable] = candidate_types[0]

        return types.Substitution(sub)

    def defaulted_predicates(self, type_variables, predicates):
        ambiguities = self.find_ambiguities(type_variables, predicates)

        results = []
        for a in ambiguities:
            candidate_types = self.candidates(a)
            if len(candidate_types) == 0:
                raise types.TypeError(f'No default type for {a.type_variable} {predicates}')

            # Update the substitution to record what the type was defaulted to
            self.unify_types(
                types.TVariable(a.type_variable),
                candidate_types[0]
            )
            # Record the fact that those predicates were handled
            results.extend(a.predicates)

        return results

    def candidates(self, ambiguity):
        ''' Find concrete types that are a candidate for an ambiguous type. '''
        assert(isinstance(ambiguity, Ambiguity))

        for pred in ambiguity.predicates:
            # Only predicates on type variables are resolvable ambiguities
            if not isinstance(pred.t, types.TVariable):
                return []

        class_names = set()
        for p in ambiguity.predicates:
            for p_super in self.get_predicates_for_superclasses(p):
                class_names.add(p_super.tclass.name)

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

        class_def = self.class_definitions[predicate.tclass.name]
        for superclass in class_def.supers:
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
            other_predicates = results + predicates[i+1:]
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

    def unify_types(self, t1: types.Type, t2: types.Type):
        ''' Updates the substitution to make the types equal '''
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
        self.assumptions = assumptions or {}  # type: typing.Dict[str, types.Scheme]
        self.parent = parent  # type: None | Assumptions

    def define(self, name, scheme: types.Scheme):
        self.assumptions[name] = scheme

    def get_scheme(self, name) -> types.Scheme:
        if name in self.assumptions:
            return self.assumptions[name]
        elif self.parent:
            return self.parent.get_scheme(name)
        else:
            raise types.TypeError(f'No type for {name}')

    def make_child(self, assumptions=None):
        return Assumptions(assumptions=assumptions, parent=self)

    def free_type_vars(self) -> types.FreeTypeVariables:
        tvars = types.FreeTypeVariables()
        for scheme in self.assumptions.values():
            tvars |= scheme.free_type_vars()
        if self.parent is not None:
            tvars |= self.parent.free_type_vars()
        return tvars

    def apply(self, substitution: types.Substitution):
        return Assumptions(
            {
                name: scheme.apply(substitution)
                for (name, scheme) in self.assumptions.items()
            },
            parent=self.parent and self.parent.apply(substitution)
        )


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
