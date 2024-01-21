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


from interpreter.syntax import (
    Declaration, DFunction, ClassDef, InstanceDef, StructDef,
    Expression,
    ELambda, ELiteral, ECall, EConstruct, EIf,
    EPartial, ELet, ELambda, Binding, EVariable,
    MethodDecl, EAccess,
)

from interpreter.types import (
    Qualified, Type, Substitution, TClass, TypeError,
    Predicate, TConstructor, TApplication, TVariable,
    TypeVariable,
    match, make_function_type, require_function_type,
)


class LoweringInput:
    ''' This contains the inputs to the lowering pass '''

    def __init__(self, declarations: list, structs: list, classes: list, instances: list):
        self.declarations = declarations
        self.structs = structs
        self.classes = classes
        self.instances = instances

    def lower(self):
        ''' returns a LoweringOutput containing the results '''
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

        return LoweringOutput(
            dictionary_functions + lowered_declarations,
            dictionaries + self.structs
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


        return StructDef(dictionary_name, [tv.type_variable], fields)

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
            body
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

        if isinstance(expression, ELet):
            # Add the names defined in the let block as local variables
            # (potentially shadowing other definitions) before handling
            # dictionary passing in the body or binding expressions.
            # (bindings are allowed to refer to each other)
            binding_names = [b.name for b in expression.bindings]
            lambda_context = context.for_method(binding_names, [])

            bindings = [
                Binding(b.name, self._lower_expression(lambda_context, b.value))
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
                # If it's a local variable, leave it unchanged.
                # TODO: This is incorrect if a local let binding is allowed to
                # have class constrains (which they are in Haskell). Fixing
                # this will require adding a Qualified to let bindings.
                return expression

            declaration = context.get_from_scope(name)
            if declaration is not None:
                # Handle references to other functions
                # (these need dictionaries to be passed)
                return self._rewrite_static_reference(context, declaration, expression)

            method = context.find_class_method(name)
            if method is not None:
                # Handle references to methods on a class
                # (these come from dictionaries - and may need dictionaries of their own)
                return self._rewrite_class_call(context, method, expression)

            raise RuntimeError(f'could not find definition of name {name}')

        raise NotImplementedError(f'_lower_expression should handle {repr(expression)}')

    def _rewrite_static_reference(self, context, declaration: DFunction, expression: EVariable):
        ''' Rewrite static references to other functions.

        If the function `foo` takes has a predicate `(Show a)`, this would
        convert the variable `foo` into the partial application
        `(foo make__Show())`
        '''

        qualified = declaration.t
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


class LoweringOutput:
    ''' The output of the lowering pass '''

    # declarations: list[Declaration], dictionaries: list[StructDef]
    def __init__(self, declarations: list, dictionaries: list):
        self.declarations = declarations
        self.dictionaries = dictionaries

    def __eq__(self, o):
        return (
            isinstance(o, LoweringOutput) and
            o.declarations == self.declarations and
            o.dictionaries == self.dictionaries
        )

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f'LoweringOutput({repr(self.declarations)}, {repr(self.dictionaries)})'

    def to_lisp(self):
        lisp = [d.to_lisp() for d in self.declarations]
        lisp += [s.to_lisp() for s in self.dictionaries]
        return lisp


class Method:
    ''' represents a method in a class definition '''

    def __init__(self, name, tclass, qualified):
        self.name = name
        self.tclass = tclass
        self.qualified = qualified


class Context:
    ''' Internal data used during the lowering process '''

    # local_vars: list[str]
    # dict_preds: dict[str, Predicate] (str is the name of the dictionary)
    # scope: dict[str, Declaration] (top-level function declarations)
    # methods: dict[str, tuple[TClass, Qualified]] (methods that appear in classes)
    # instances: list[InstanceDef]
    # classes: list[ClassDef]
    def __init__(self, local_vars, dict_preds, scope, methods, instances, classes):
        self.local_vars = local_vars
        self.dict_preds = dict_preds
        self.scope = scope
        self.methods = methods
        self.instances = instances
        self.classes = classes

    @classmethod
    def build(cls, classes, instances, declarations):
        # TODO: Add built-ins to the locals
        locals = ['inc', 'join', 'concat', 'length', '+', '*', '-', '/']

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
            dict_preds={},
            scope=scope,
            methods=methods,
            instances=instances,
            classes=classes,
        )

    def for_method(self, arg_names, predicates):
        new_locals = arg_names + self.local_vars

        new_dict_preds = {**self.dict_preds}
        for pred in predicates:
            name = _predicate_to_arg_name(pred)
            new_dict_preds[name] = pred

        return Context(
            local_vars=new_locals,
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
            recursive_super_expr = self._find_super(super_class, predicate, super_expr)
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

            substitution = match(instance.get_type(), predicate.t)
            if substitution is not None:
                return (substitution, instance)

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
