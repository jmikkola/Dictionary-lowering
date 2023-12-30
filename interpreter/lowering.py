# module lowering


from interpreter.syntax import (
    Declaration, DFunction, ClassDef, InstanceDef,
    Expression,
    ELambda, TypedExpression, ELiteral, EParen, ECall,
    EPartial, ELet, ELambda, Binding, EVariable,
    MethodDecl,
)

from interpreter.types import (
    Qualified, Type, Substitution, TClass, TypeError,
    Predicate, TConstructor, TApplication,
    match,
)


class LoweringInput:
    ''' This contains the inputs to the lowering pass '''

    def __init__(self, declarations: list, classes: list, instances: list):
        self.declarations = declarations
        self.classes = classes
        self.instances = instances

    def lower(self):
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
            dictionaries
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

        # Remove the qualification from the method types because classes don't
        # exist in the output code. The actual implementations will also get
        # lowered so that they don't need predicates.
        method_fields = [
            (method_decl.name, method_decl.get_type())
            for method_decl in class_def.methods
        ]

        fields = super_fields + method_fields

        return Dictionary(
            dictionary_name,
            type_variables,
            fields
        )

    def _to_super_field(self, tclass, tv):
        # Prefix the field's name with 'super' to note the fact that this
        # references another dictionary
        name = "super" + tclass.name
        # The type of the field is the superclass's dictionary type.
        #
        # It uses the same type variable as the current dictionary because the
        # superclass instance will be on the same concrete type.
        #
        # This isn't concerned with what other classes a particular instance
        # might depend upon.
        dictionary_name = _class_to_dictionary_name(tclass)
        t = TApplication(TConstructor(dictionary_name), TVariable(tv))
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
            TApplication(
                TConstructor(_class_to_dictionary_name(predicate.tclass)),
                predicate.t
            )
            for predicate in predicates
        ]

        return TApplication(
            function_type.t,
            new_argument_types + function_type.args
        )


    def _make_dictionary_fn(self, context, instance):
        ''' Creates a function for constructing a instance's dictionary '''

        # Arg names for the function that produces the dictionary
        arg_names = [
            _predicate_to_arg_name(p)
            for p in instance.get_predicates()
        ]

        class_def = context.get_class_def(instance.get_class())

        inst_t = instance.get_type()
        method_context = context.for_method(arg_names, instance.get_predicates())
        lambdas = [
            self._declaration_to_lambda(method_context, class_def, inst_t, method)
            for method in instance.method_impls
        ]

        # TODO

    def _declaration_to_lambda(self, context, class_def, inst_type, method_impl):
        assert(isinstance(method_impl, DFunction))

        method_decl = class_def.get_method(method_impl.name)
        substitution = Substitution.singleton(class_def.tvar, inst_type)

        method_type = method_decl.get_type().apply(substitution)

        # Arg names for the lambda function stored inside the dictionary
        arg_names = method_impl.arg_names
        lambda_context = context.for_method(arg_names, [])

        body = self._lower_expression(lambda_context, method_impl.body)

        l = ELambda(arg_names, body)
        return TypedExpression(l, method_type)

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

        if isinstance(expression, TypedExpression):
            expr = self._lower_expression(context, expression.expr)
            return TypedExpression(expr, expression.t)

        if isinstance(expression, EParen):
            new_inner = self._lower_expression(context, expression.inner)
            return EParen(new_inner)

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

            return ECall(f_expr, arg_exprs)

        if isinstance(expression, ELet):
            bindings = [
                Binding(b.name, self._lower_expression(context, b.value))
                for b in expression.bindings
            ]
            binding_names = [b.name for b in bindings]

            # Add the names defined in the let block as local variables
            # (potentially shadowing other definitions) before handling
            # dictionary passing in the body.
            lambda_context = context.for_method(binding_names, [])
            inner = self._lower_expression(lambda_context, expression.inner)

            return ELet(bindings, inner)

        if isinstance(expression, ELambda):
            lambda_context = context.for_method(expression.arg_names, [])
            body = self._lower_expression(lambda_context, expression.body)
            return ELambda(expression.arg_names, body)

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
                # (these come from dictionaries)
                return self._rewrite_class_call(context, method, expression)

            raise RuntimeError(f'could not find definition of name {name}')

        raise NotImplementedError(f'_lower_expression should handle {repr(expression)}')

    def _rewrite_static_reference(self, context, declaration: DFunction, expression: EVariable):
        qualified = declaration.t
        # TODO: expression doesn't _have_ a `.t` right now
        try:
            substitution = match(qualified.t, expression.t)
        except TypeError as e:
            raise TypeError(f'type error in using {expression}: {e}')

        predicates = substitution.apply_to_list(qualified.predicates)
        dictionary_args = []

        for pred in predicates:
            dictionary = context.lookup_dictionary(pred)
            dictionary_args.append(dictionary)

        return EPartial(expression, dictionary_args)

    def _rewrite_class_call(self, context, method: MethodDecl, expression: EVariable):
        # TODO: expression don't _have_ a `.t` right now
        instance_type = context.find_instance_type(method, expression.t)
        # TODO: what goes in the predicate
        dictionary = context.lookup_dictionary(Predicate())
        get_dict_field = ECall(EVariable(expression.name), dictionary)
        return EParen(get_dict_field)


class LoweringOutput:
    ''' The output of the lowering pass '''

    # declarations: list[Declaration], dictionaries: list[Dictionary]
    def __init__(self, declarations: list, dictionaries: list):
        self.declarations = declarations
        self.dictionaries = dictionaries


class Dictionary:
    ''' Represents the struct used to pass class methods around '''

    # name: str, type_vars: list[str], fields: list[tuple[str, Type]]
    def __init__(self, name: str, type_vars: list, fields: list):
        ''' fields contains both methods and superclasses '''
        self.name = name
        self.type_vars = type_vars
        self.fields = fields

    def __str__(self):
        tvs = ''
        if len(self.type_vars) > 0:
            tvs = '<' + ' '.join(str(tv) for tv in self.type_vars) + '>'

        header = f'struct {self.name}{tvs} {{'
        lines = [header]
        for (name, t) in self.fields:
            lines.append(f'  {name}: {type},')

        lines.append('}')
        return '\n'.join(lines)


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
        locals = []

        scope = {
            d.name: d
            for d in declarations
        }

        methods = {
            method.name: Method(method.name, classdef.tclass, method.qualified)
            for classdef in classes
            for method in classdef.methods
        }

        return Context(
            locals=locals,
            dict_preds={},
            scope=scope,
            methods=methods,
            instances=instances,
            classes=classes,
        )

    def for_method(self, arg_names, predicates):
        new_locals = arg_names + self.locals

        new_dict_preds = {**self.dict_preds}
        for pred in predicates:
            name = _predicate_to_arg_name(pred)
            new_dict_preds[name] = pred

        return Context(
            locals=new_locals,
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
        return var_name in self.locals

    def get_from_scope(self, var_name: str):
        ''' returns Declaration or None '''
        return self.scope.get(var_name)

    def lookup_dictionary(self, predicate):
        pass
        # TODO: lots of logic here


def _predicate_to_arg_name(predicate):
    class_name = predicate.tclass.name
    return f'dict_{class_name}_{predicate.t}'

def _class_to_dictionary_name(tclass):
    return tclass.name + "Methods"
