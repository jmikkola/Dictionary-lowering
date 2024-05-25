# module lowering

"""The lowering package replaces typeclasses with dictionaries.

This takes in code that contains and uses classes and instances of classes. It
outputs equivalent code that doesn't have any classes.

Classes become "dictionaries" (structs) that have fields for holding the
functions defined in a class. Instances become functions for producing values
of those dictionaries. Class predicates on methods become additional arguments
to those methods where the dictionaries are passed in.

This package assumes a few things about the input code:

- It is well-typed.
- All expressions are annotated with types.
- Validations have been done (e.g. classes referenced actually exist, class
  hierarchies don't contain cycles)
"""

import typing

from interpreter import builtin

from interpreter.syntax import (
    DFunction, InstanceDef, StructDef,
    Expression,
    ELambda, ELiteral, ECall, EConstruct, EIf,
    EPartial, ELet, Binding, EVariable,
    EAccess,
)

from interpreter.types import (
    Type, Substitution, TClass, TypeError,
    Predicate, TConstructor, TApplication, TVariable,
    match, make_function_type, require_function_type,
    Qualified,
)

from interpreter.program import Program


def lower(program: Program) -> Program:
    ''' Lower the program to remove classes and instances '''
    input = LoweringInput(program)
    return input.lower()


class LoweringInput:
    ''' This contains the inputs to the lowering pass '''

    def __init__(self, program: Program):
        self.declarations = program.functions
        self.structs = program.structs
        self.classes = program.classes
        self.instances = program.instances

        self.builtin_functions = set()  # type: typing.Set[str]

        self._add_builtins()

    def _add_builtins(self):
        self.classes.extend(builtin.get_classes())
        self.instances.extend(builtin.get_instances())

        for name in builtin.get_function_types():
            self.builtin_functions.add(name)

    def lower(self):
        ''' returns a Program containing the results '''
        context = Context.build(self.classes, self.instances, self.declarations)

        dictionary_functions = [
            self._make_dictionary_fn(context, instance)
            for instance in self.instances
        ]

        lowered_declarations = [
            self._lower_function(context, declaration)
            for declaration in self.declarations
        ]

        dictionaries = [
            self._make_dictionary(class_def)
            for class_def in self.classes
        ]

        return Program(
            from_stage='lowering',
            functions=dictionary_functions + lowered_declarations,
            structs=dictionaries + self.structs,
            classes=[],
            instances=[],
        )

    def _make_dictionary(self, class_def):
        ''' Creates the dictionary structure for a class '''

        dictionary_name  = _class_to_dictionary_name(class_def.tclass)

        # Create fields for referencing the dictionaries of superclasses.
        tv = TVariable(class_def.tvar)
        super_fields = [
            self._to_super_field(super_class, tv)
            for super_class in class_def.supers
        ]

        method_fields = [
            (method_decl.method_name, self._predicates_to_arg_types(method_decl.qual_type))
            for method_decl in class_def.methods
        ]

        fields = super_fields + method_fields

        # I'm not convinced yet that it's actually necessary to keep the other
        # type variables.
        # If they are kept, it becomes a challenge to ensure they are used in
        # the right order.
        # type_variables = set()
        # for method in class_def.methods:
        #     method_type = method.get_type()
        #     type_variables |= method_type.free_type_vars()


        return StructDef(
            dictionary_name,
            [tv.type_variable],
            fields,
            is_builtin=class_def.is_builtin
        )

    def _to_super_field(self, tclass, tv):
        # Prefix the field's name with 'super' to note the fact that this
        # references another dictionary
        name = _to_super_field_name(tclass)
        # The type of the field is the superclass's dictionary type.
        #
        # It uses the same type variable as the current dictionary because the
        # superclass instance will be on the same concrete type.
        #
        # This isn't concerned with what other classes a particular instance
        # might depend upon.
        dictionary_name = _class_to_dictionary_name(tclass)
        t = TApplication(TConstructor(dictionary_name), [tv])
        return (name, t)

    def _lower_function(self, context, declaration):
        ''' Converts the function's predicates to dictionary arguments '''

        qualified = declaration.t

        decl_type = self._predicates_to_arg_types(qualified)

        new_arg_names = [_predicate_to_arg_name(p) for p in qualified.predicates]

        method_context = context.for_method(
            declaration.arg_names,
            qualified.predicates
        )
        body = self._lower_expression(method_context, declaration.body)

        return DFunction(
            declaration.name,
            decl_type,
            new_arg_names + declaration.arg_names,
            body,
            is_builtin=declaration.is_builtin
        )

    def _predicates_to_arg_types(self, qualified):
        '''Convert predicates to argument types.

        Converts a function type with predicates to a
        function type without predicates (but with extra
        arguments)
        '''

        predicates = qualified.predicates
        function_type = qualified.t

        # This code assumes that the function's type is Fn applied to some type
        # arguments
        assert(isinstance(function_type, TApplication))
        assert(function_type.t == TConstructor('Fn'))

        # Converts e.g. `(Show a) =>` to an argument type `ShowMethods<a>`
        new_argument_types = [
            _pred_type_to_arg_type(predicate)
            for predicate in predicates
        ]

        return TApplication(
            function_type.t,
            new_argument_types + function_type.args
        )

    def _make_dictionary_fn(self, context, instance):
        ''' Creates a function for constructing a instance's dictionary '''

        inst_t = instance.get_type()
        tclass = instance.get_class()
        predicates = instance.get_predicates()

        class_def = context.get_class_def(tclass)

        # Construct the type of the function that returns the dictionary
        arg_types = [
            _pred_type_to_arg_type(predicate)
            for predicate in predicates
        ]
        dictionary_type_name = _class_to_dictionary_name(tclass)
        struct_type = TApplication(
            TConstructor(dictionary_type_name),
            [inst_t]
        )
        function_type = make_function_type(arg_types, struct_type)

        # Determine the names
        function_name = _instance_constructor_name(instance)
        arg_names = [
            _predicate_to_arg_name(predicate)
            for predicate in predicates
        ]

        # Create the body of the function which instantiates the structure
        lambda_context = context.for_method(arg_names, predicates)
        lambdas = [
            self._declaration_to_lambda(lambda_context, class_def, inst_t, method)
            for method in instance.method_impls
        ]

        super_dictionaries = [
            self._to_super_dictionary(lambda_context, inst_t, super_class)
            for super_class in class_def.supers
        ]

        struct_expr = EConstruct(
            struct_type,
            dictionary_type_name,
            super_dictionaries + lambdas
        )

        return DFunction(
            name=function_name,
            t=function_type,
            arg_names=arg_names,
            body=struct_expr,
            is_builtin=instance.is_builtin,
        )

    def _to_super_dictionary(self, context, t, tclass):
        predicate = Predicate(tclass, t)
        return context.lookup_dictionary([], predicate)

    def _declaration_to_lambda(self, context, class_def, inst_type, method_impl):
        assert(isinstance(method_impl, DFunction))

        method_decl = class_def.get_method(method_impl.name)
        substitution = Substitution.singleton(class_def.tvar, inst_type)

        method_type = method_decl.get_type().apply(substitution)

        # Arg names for the lambda function stored inside the dictionary
        arg_names = method_impl.arg_names
        lambda_context = context.for_method(arg_names, [])

        body = self._lower_expression(lambda_context, method_impl.body)

        return ELambda(t=method_type, arg_names=arg_names, body=body)

    def _lower_expression(self, context, expression: Expression) -> Expression:
        '''Lowers expressions.

        Converts the expression to one that doesn't rely
        on classes, and instead directly passes (and
        uses) dictionaries to get the instance methods
        needed.
        '''

        # Trivial cases:

        if isinstance(expression, ELiteral):
            # Literal values don't need lowering
            return expression

        # Recursive cases:

        if isinstance(expression, ECall):
            # If f_expr is a function that needs dictionaries passed, this
            # will partially apply those arguments to it.
            f_expr = self._lower_expression(context, expression.f_expr)
            # Likewise for the arguments
            arg_exprs = [
                self._lower_expression(context, arg_expr)
                for arg_expr in expression.arg_exprs
            ]

            # Remove unneeded partial application
            if isinstance(f_expr, EPartial):
                dictionary_args = f_expr.arg_exprs
                arg_exprs = dictionary_args + arg_exprs
                f_expr = f_expr.f_expr

            return ECall(expression.get_type(), f_expr, arg_exprs)

        if isinstance(expression, EPartial):
            f_expr = self._lower_expression(context, expression.f_expr)
            arg_exprs = [
                self._lower_expression(context, arg_expr)
                for arg_expr in expression.arg_exprs
            ]
            return EPartial(expression.get_type(), f_expr, arg_exprs)

        if isinstance(expression, ELet):
            # Add the names defined in the let block as local variables
            # (potentially shadowing other definitions) before handling
            # dictionary passing in the body or binding expressions.
            # (bindings are allowed to refer to each other)
            binding_names = [b.name for b in expression.bindings]
            binding_predicates = [p for b in expression.bindings for p in b.get_predicates()]

            lambda_context = context.for_method(binding_names, binding_predicates)
            for binding in expression.bindings:
                lambda_context.set_local_type(binding.name, binding.t)

            bindings = [
                self._lower_let_binding(lambda_context, b)
                for b in expression.bindings
            ]
            inner = self._lower_expression(lambda_context, expression.inner)

            return ELet(expression.get_type(), bindings, inner)

        if isinstance(expression, ELambda):
            lambda_context = context.for_method(expression.arg_names, [])
            body = self._lower_expression(lambda_context, expression.body)
            return ELambda(expression.get_type(), expression.arg_names, body)

        if isinstance(expression, EIf):
            test = self._lower_expression(context, expression.test)
            if_case = self._lower_expression(context, expression.if_case)
            else_case = self._lower_expression(context, expression.else_case)
            return EIf(expression.get_type(), test, if_case, else_case)

        if isinstance(expression, EAccess):
            lhs = self._lower_expression(context, expression.lhs)
            return EAccess(expression.get_type(), lhs, expression.field)

        if isinstance(expression, EConstruct):
            arg_exprs = [
                self._lower_expression(context, e)
                for e in expression.arg_exprs
            ]
            return EConstruct(expression.get_type(), expression.struct_name, arg_exprs)

        # Where dictionary passing is actually added:

        if isinstance(expression, EVariable):
            name = expression.name
            # The variable might reference something with class constraints.
            if context.is_local(name):
                qualified = context.get_local_type(name)
                if qualified is not None:
                    # Variable comes from a let binding and potentially can
                    # take dictionary arguments
                    return self._rewrite_static_reference(context, qualified, expression)
                else:
                    # Variable comes from an argument and can't take dictionary arguments
                    return expression

            if name in self.builtin_functions:
                return expression

            declaration = context.get_from_scope(name)
            if declaration is not None:
                # Handle references to other functions
                # (these need dictionaries to be passed)
                return self._rewrite_static_reference(context, declaration.t, expression)

            method = context.find_class_method(name)
            if method is not None:
                # Handle references to methods on a class
                # (these come from dictionaries - and may need dictionaries of their own)
                return self._rewrite_class_call(context, method, expression)

            raise RuntimeError(f'could not find definition of name {name}')

        raise NotImplementedError(f'_lower_expression should handle {repr(expression)}')

    def _lower_let_binding(self, context, binding: Binding):
        value = binding.value
        if isinstance(value, ELambda) and binding.t is not None:
            # Potentially add dictionary variables to the bound lambda function
            qualified = binding.t

            decl_type = self._predicates_to_arg_types(qualified)

            new_arg_names = [_predicate_to_arg_name(p) for p in qualified.predicates]

            lambda_context = context.for_method(
                value.arg_names,
                qualified.predicates
            )

            body = self._lower_expression(lambda_context, value.body)

            lowered = ELambda(
                t=decl_type,
                arg_names=new_arg_names + value.arg_names,
                body=body
            )  # type: Expression
            t = Qualified([], decl_type)
        else:
            lowered = self._lower_expression(context, value)
            t = binding.t

        return Binding(binding.name, lowered, t=t)

    def _rewrite_static_reference(self, context, qualified: Qualified, expression: EVariable):
        ''' Rewrite static references to other functions.

        If the function `foo` takes has a predicate `(Show a)`, this would
        convert the variable `foo` into the partial application
        `(foo make__Show())`
        '''

        try:
            # Find out how the function's type was instantiated here
            substitution = match(qualified.t, expression.get_type())
        except TypeError as e:
            raise TypeError(f'type error in using {expression}: {e}')

        # Determine what concrete types the predicates now have
        predicates = substitution.apply_to_list(qualified.predicates)
        dictionary_args = []

        # Get the dictionaries for those predicates
        for pred in predicates:
            dictionary = context.lookup_dictionary_all(pred)
            dictionary_args.append(dictionary)

        expr_type = expression.get_type()
        if predicates:
            expr_arg_types, expr_return_type = require_function_type(expr_type)
            # Create a new type for `expression` now that predicates are passed
            # as additional args
            predicate_arg_types = [arg.get_type() for arg in dictionary_args]
            arg_types = predicate_arg_types + expr_arg_types
            result_type = make_function_type(arg_types, expr_return_type)
        else:
            # No predicates means no change to the type is needed
            result_type = expr_type

        variable = EVariable(result_type, expression.name)

        if not dictionary_args:
            return variable

        return EPartial(expression.get_type(), variable, dictionary_args)

    def _rewrite_class_call(self, context, method, expression: EVariable):
        ''' Rewrites a reference to a method from a class.

        method is a Method not a MethodDecl.

        If the method `show` (from the class `Show`) is referenced, this will
        convert that to extracting the field `show` from the `Show` dict,
        complete with logic to find or create that dictionary.
        '''
        t = expression.get_type()
        instance_substitution = context.find_instance_substitution(method, t)

        class_type_var = context.get_class_def(method.tclass).tvar
        instance_type = instance_substitution[class_type_var]

        predicate = Predicate(method.tclass, instance_type)
        dictionary = context.lookup_dictionary_all(predicate)

        # This is additional predicates on the particular method, e.g. (Eq a)
        # on the `elem` method from `Foldable`.
        method_predicates = method.qualified.apply(instance_substitution).predicates
        if method_predicates:
            dictionary_args = [
                context.lookup_dictionary_all(p)
                for p in method_predicates
            ]

            expr_arg_types, expr_return_type = require_function_type(t)
            # Create a new type for `expression` now that predicates are passed
            # as additional args
            predicate_arg_types = [arg.get_type() for arg in dictionary_args]
            arg_types = predicate_arg_types + expr_arg_types
            result_type = make_function_type(arg_types, expr_return_type)
        else:
            dictionary_args = []
            result_type = t

        access = EAccess(result_type, dictionary, expression.name)

        if not dictionary_args:
            return access

        return EPartial(t, access, dictionary_args)


class Method:
    ''' represents a method in a class definition '''

    def __init__(self, name, tclass, qualified):
        self.name = name
        self.tclass = tclass
        self.qualified = qualified


class Context:
    ''' Internal data used during the lowering process '''

    # local_vars: list[str]
    # local_types: dict[str, Qualified]
    # dict_preds: dict[str, Predicate] (str is the name of the dictionary)
    # scope: dict[str, Declaration] (top-level function declarations)
    # methods: dict[str, tuple[TClass, Qualified]] (methods that appear in classes)
    # instances: list[InstanceDef]
    # classes: list[ClassDef]
    def __init__(self, local_vars, local_types, dict_preds, scope, methods, instances, classes):
        self.local_vars = local_vars
        self.local_types = local_types
        self.dict_preds = dict_preds
        self.scope = scope
        self.methods = methods
        self.instances = instances
        self.classes = classes

    @classmethod
    def build(cls, classes, instances, declarations):
        locals = builtin.NON_TYPECLASS_NAMES

        scope = {
            d.name: d
            for d in declarations
        }

        methods = {
            method.method_name: Method(method.method_name, classdef.tclass, method.qual_type)
            for classdef in classes
            for method in classdef.methods
        }

        return Context(
            local_vars=locals,
            local_types={},
            dict_preds={},
            scope=scope,
            methods=methods,
            instances=instances,
            classes=classes,
        )

    def for_method(self, arg_names, predicates):
        new_locals = arg_names + self.local_vars

        new_local_types = {**self.local_types}

        new_dict_preds = {**self.dict_preds}
        for pred in predicates:
            name = _predicate_to_arg_name(pred)
            new_dict_preds[name] = pred

        return Context(
            local_vars=new_locals,
            local_types=new_local_types,
            dict_preds=new_dict_preds,
            scope=self.scope,
            methods=self.methods,
            instances=self.instances,
            classes=self.classes,
        )

    def get_class_def(self, tclass: TClass):
        for class_def in self.classes:
            if class_def.tclass == tclass:
                return class_def

    def is_local(self, var_name: str):
        return var_name in self.local_vars

    def get_local_type(self, var_name: str):
        # This will return None if var_name refers to an argument
        return self.local_types.get(var_name)

    def set_local_type(self, var_name: str, qualified: Qualified):
        self.local_types[var_name] = qualified

    def get_from_scope(self, var_name: str):
        ''' returns Declaration or None '''
        return self.scope.get(var_name)

    def find_class_method(self, name):
        ''' returns Method or None '''
        return self.methods.get(name)

    def find_instance_substitution(self, method, called_type):
        ''' Find the substitution for the instance type used.

        This substitution shows how to change the type variables in a
        class definitiong to the types (concrete or variable) used in
        the callsite.
        '''
        return match(method.qualified.t, called_type)

    def lookup_dictionary_all(self, predicate):
        ''' Look up the dictionary using all predicates in scope.

        In the body of a normal function, this will be the predicates
        on that function.
        '''
        predicates_in_scope = [
            (predicate, name)
            for (name, predicate) in self.dict_preds.items()
        ]
        return self.lookup_dictionary(predicates_in_scope, predicate)

    def lookup_dictionary(self, predicates_in_scope, predicate):
        t = predicate.t

        if isinstance(t, TVariable):
            # If the type is still a variable (and typechecking hasn't replaced
            # it with a concrete type), that means that the entire function is
            # still abstract with respect to that type. It also means that the
            # dictionary must come from an argument to that function (either
            # directly, or a superclass of one of the classes passed in).
            return self._find_dictionary_for_predicate(predicates_in_scope, predicate)

        # If it's not a type variable, find an instance for that concrete type,
        # and get the dictionary for that instance (e.g. the instance for `Show
        # [a]` if the concrete type is [Int]`).
        (substitution, instance) = self._find_matching_instance(predicate)
        instance_fn = self._instance_to_dict_fn(instance)

        # That instance may depend on other classes (e.g. `Show a => Show [a]`).
        # Recursively look up those dictionaries.
        instance_predicates = substitution.apply_to_list(instance.get_predicates())
        instance_predicate_args = [
            self.lookup_dictionary(predicates_in_scope, p)
            for p in instance_predicates
        ]

        return_type = _return_type_from_fn_type(instance_fn.get_type())

        return ECall(return_type, instance_fn, instance_predicate_args)

    def _find_dictionary_for_predicate(self, predicates_in_scope, predicate):
        ''' Return an expression that uses one of the passed-in dictionaries.

        This expression might directly reference it or might have to take steps
        to extract the superclass field.
        '''
        # Loop over the predicates that dictionaries are passed in for.
        # See if either it's possible to use one directly or get a superclass from it.
        for (in_scope_p, name) in predicates_in_scope:
            # Create an expression for the current dictionary's variable
            typ = TApplication(
                TConstructor(_class_to_dictionary_name(in_scope_p.tclass)),
                [in_scope_p.t]
            )
            expr = EVariable(typ, name)

            if in_scope_p == predicate:
                # the passed-in dictionary works
                return expr

            # Look for superclasses of the class in `in_scope_p` that might work:
            super_expr = self._find_super(in_scope_p, predicate, expr)
            if super_expr is not None:
                return super_expr

        raise RuntimeError(f'could not find dictionary for predicate {predicate}')

    def _find_super(self, in_scope_p, predicate, expr):
        ''' See if `predicate` matches any superclass of `in_scope_p` '''
        class_def = self.get_class_def(in_scope_p.tclass)
        if class_def is None:
            raise RuntimeError(f'expected to have a definition for the class {in_scope_p.tclass}')
        super_classes = class_def.supers

        for super_class in super_classes:
            super_pred = Predicate(super_class, in_scope_p.t)

            dictionary_name = _class_to_dictionary_name(super_class)
            t = TApplication(TConstructor(dictionary_name), [predicate.t])

            field_name = _to_super_field_name(super_class)

            super_expr = EAccess(t, expr, field_name)

            if super_pred == predicate:
                # Match found. Take the existing dictionary and get this
                # superclass out of it.
                return super_expr

            # Check superclasses recursively
            recursive_super_expr = self._find_super(super_pred, predicate, super_expr)
            if recursive_super_expr is not None:
                return recursive_super_expr

        return None

    def _find_matching_instance(self, predicate):
        ''' Find an instance that matches the predicate

        Look for instances for (a) the same class as the predicate
        and (b) the type of the instance can match the type in the
        predicate.
        '''
        for instance in self.instances:
            if instance.get_class() != predicate.tclass:
                continue

            try:
                substitution = match(instance.get_type(), predicate.t)
                return (substitution, instance)
            except TypeError:
                continue

        raise RuntimeError(f'could not find matching instance for {predicate}')

    def _instance_to_dict_fn(self, instance):
        '''
        Create the var that references the function that creates the dictionary
        for this particular instance.
        '''
        name = _instance_constructor_name(instance)

        tclass = instance.get_class()
        dictionary_type = TApplication(
            TConstructor(_class_to_dictionary_name(tclass)),
            [instance.get_type()]
        )
        arg_types = [
            _pred_type_to_arg_type(p)
            for p in instance.get_predicates()
        ]
        t = make_function_type(arg_types, dictionary_type)

        return EVariable(t, name)


def _predicate_to_arg_name(predicate: Predicate) -> str:
    class_name = predicate.tclass.name
    return f'dict_{class_name}_{predicate.t}'


def _class_to_dictionary_name(tclass: TClass) -> str:
    return tclass.name + "Methods"


def _instance_constructor_name(instance: InstanceDef) -> str:
    dict_name = _class_to_dictionary_name(instance.get_class())
    type_name = str(instance.get_type())
    return f'make__{dict_name}__{type_name}'


def _pred_type_to_arg_type(predicate):
    ''' Converts e.g. `(Show a) =>` to an argument type `ShowMethods<a>` '''
    return TApplication(
        TConstructor(_class_to_dictionary_name(predicate.tclass)),
        [predicate.t]
    )


def _to_super_field_name(tclass):
    return "super" + tclass.name


def _return_type_from_fn_type(t: Type) -> Type:
    '''Assuming t is a function type, this returns the return type
    of that function'''

    assert(isinstance(t, TApplication))
    assert(t.t == TConstructor('Fn'))
    return t.args[-1]
