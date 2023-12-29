# module lowering


from interpreter.syntax import (
    Declaration, DFunction, ClassDef, InstanceDef,
    ELambda, TypedExpression,
)

from interpreter.types import Qualified, Type, Substitution


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

        # TODO
        lowered_declarations = []

        # TODO
        dictionaries = []

        return LoweringOutput(
            dictionary_functions + lowered_declarations,
            dictionaries
        )

    def _make_dictionary_fn(self, context, instance):
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

    def _lower_expression(self, context, expression):
        if isinstance(expression, TypedExpression):
            expr = self._lower_expression(context, expression.expr)
            return TypedExpression(expr, expression.t)

        # TODO: other expression types


class LoweringOutput:
    ''' The output of the lowering pass '''

    # declarations: list[Declaration], dictionaries: list[Dictionary]
    def __init__(self, declarations: list, dictionaries: list):
        self.declarations = declarations
        self.dictionaries = dictionaries


class Dictionary:
    ''' Represents the struct used to pass class methods around '''

    # name: str, type_vars: list[str], method_fields: list[tuple[str, Type]]
    def __init__(self, name: str, type_vars: list, method_fields: list):
        self.name = name
        self.type_vars = type_vars
        self.method_fields = method_fields

    def __str__(self):
        tvs = ''
        if len(self.type_vars) > 0:
            tvs = '<' + ' '.join(str(tv) for tv in self.type_vars) + '>'

        header = f'struct {self.name}{tvs} {{'
        lines = [header]
        for (name, t) in self.method_fields:
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

    def get_class_def(self, tclass):
        for class_def in self.classes:
            if class_def.tclass == tclass:
                return class_def


def _predicate_to_arg_name(predicate):
    class_name = predicate.tclass.name
    return f'dict_{class_name}_{predicate.t}'
