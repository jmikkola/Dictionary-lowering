# module treewalker

'''
This is a tree-walking interpreter
'''


from interpreter import syntax
from interpreter import types


class NameError(RuntimeError):
    pass


class Interpreter:
    def __init__(self):
        self.declarations = {}
        self.structs = {}

        self.builtins = set([
            '+', '-', '*', '/', '%',
            '>', '<', '<=', '>=', '==', '!=',
            'and', 'or', 'not',
            'str', 'print',
        ])

    def load_structs(self, structs):
        ''' Add struct definitions to the interpreter '''
        for struct in structs:
            self.structs[struct.name] = struct

    def load_declarations(self, declarations):
        ''' Add function definitions to the interpreter '''
        for decl in declarations:
            self.declarations[decl.name] = decl

    def eval_main(self):
        ''' StAart running the main function '''
        self._call_function('main', [])

    def _call_function(self, name, arg_values):
        declaration = self._get_declaration(name)
        scope = Scope.for_args(declaration.arg_names, arg_values)
        return self._eval_expression(name, scope, declaration.body)

    def _eval_expression(self, function_name, scope, expression):
        ''' This is the main evaluation function '''

        if isinstance(expression, syntax.EVariable):
            name = expression.name
            if name in self.builtins:
                return BuiltinFunction(name)
            try:
                return scope.read(name)
            except NameError:
                return FunctionValue(self._get_declaration(name))
        elif isinstance(expression, syntax.ELiteral):
            return self._literal_to_value(expression.literal)
        elif isinstance(expression, syntax.ECall):
            function = self._eval_expression(function_name, scope, expression.f_expr)
            arg_values = [
                self._eval_expression(function_name, scope, arg_expr)
                for arg_expr in expression.arg_exprs
            ]

            if isinstance(function, FunctionValue):
                decl = function.declaration
                call_scope = Scope.for_args(decl.arg_names, arg_values)
                return self._eval_expression(decl.name, call_scope, decl.body)
            elif isinstance(function, LambdaValue):
                call_scope = function.get_eval_scope(arg_values)
                return self._eval_expression('lambda', call_scope, function.body)
            elif isinstance(function, BuiltinFunction):
                return self._call_builtin(function.name, arg_values)
            else:
                raise RuntimeError(f'Tried to call {function} as a function')
        elif isinstance(expression, syntax.EConstruct):
            struct_decl = self._get_struct_decl(expression.struct_name)
            assert len(expression.arg_exprs) == len(struct_decl.fields)
            arg_values = [
                self._eval_expression(function_name, scope, arg_expr)
                for arg_expr in expression.arg_exprs
            ]

            struct_value = StructValue(expression.type, {
                field: value
                for (field, value) in zip(struct_decl.fields, arg_values)
            })
            return struct_value
        elif isinstance(expression, syntax.EPartial):
            pass # TODO
        elif isinstance(expression, syntax.EAccess):
            lhs = self._eval_expression(function_name, scope, expression.lhs)
            assert(isinstance(lhs, StructValue))
            return lhs.access(expression.field)
        elif isinstance(expression, syntax.ELet):
            pass # TODO
        elif isinstance(expression, syntax.EIf):
            test_value = self._eval_expression(function_name, scope, expression.test)
            assert(isinstance(test_value, BoolValue))
            if test_value.value:
                return self._eval_expression(function_name, scope, expression.if_case)
            else:
                return self._eval_expression(function_name, scope, expression.else_case)
        elif isinstance(expression, syntax.ELambda):
            return LambdaValue(scope, expression.type, expression.arg_names, expression.body)

        raise NotImplementedError(f'Cannot handle this expression yet: {repr(expression)}')

    def _get_declaration(self, name):
        declaration = self.declarations.get(name)
        if declaration is None:
            raise NameError(f'{name} is not defined')
        return declaration

    def _get_struct_decl(self, name):
        struct_decl = self.structs.get(name)
        if struct_decl is None:
            raise NameError(f'{name} is not a defined struct')
        return struct_decl

    def _literal_to_value(self, literal):
        if isinstance(literal, syntax.LString):
            return StringValue(literal.value)
        elif isinstance(literal, syntax.LInt):
            return IntValue(literal.value)
        elif isinstance(literal, syntax.LFloat):
            return FloatValue(literal.value)

        raise NotImplementedError(f'Cannot handle this literal: {repr(literal)}')

    def _call_builtin(self, name, arg_values):
        if name == '+':
            assert(len(arg_values) == 2)
            # TODO: handle other types
            assert(isinstance(arg_values[0], IntValue))
            assert(isinstance(arg_values[1], IntValue))
            result_value = arg_values[0].value + arg_values[1].value
            return IntValue(result_value)
        raise NotImplementedError('builtin function not implemented: ' + name)


class Scope:
    def __init__(self, variables=None, parent=None):
        if variables is None:
            variables = {}
        self.variables = variables
        self.parent = parent

    def read(self, name):
        if name in self.variables:
            return self.variables[name]
        elif self.parent is not None:
            return self.parent.read(name)
        else:
            raise NameError(f'{name} is not in scope')

    def add_variable(self, name, value):
        self.variables[name] = value

    def update(self, name, value):
        if name in self.variables:
            self.variables[name] = value
        elif self.parent is not None:
            self.parent.update(name, value)
        else:
            raise NameError(f'{name} is not in scope')

    def make_child(self, names, values):
        child = Scope(parent=self)
        for (name, value) in zip(names, values):
            child.add_variable(name, value)
        return child

    @classmethod
    def for_args(self, names, values):
        scope = Scope()
        for (name, value) in zip(names, values):
            scope.add_variable(name, value)
        return scope


class Value:
    def __str__(self):
        return repr(self)


class BoolValue(Value):
    def __init__(self, value: bool):
        self.value = value

    def get_type(self):
        return types.TConstructor('Bool')

    def __repr__(self):
        return f'BoolValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, BoolValue) and o.value == self.value


class FloatValue(Value):
    def __init__(self, value: float):
        self.value = value

    def get_type(self):
        return types.TConstructor('Float')

    def __repr__(self):
        return f'FloatValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, FloatValue) and o.value == self.value


class IntValue(Value):
    def __init__(self, value: int):
        self.value = value

    def get_type(self):
        return types.TConstructor('Int')

    def __repr__(self):
        return f'IntValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, IntValue) and o.value == self.value


class StringValue(Value):
    def __init__(self, value: str):
        self.value = value

    def get_type(self):
        return types.TConstructor('String')

    def __repr__(self):
        return f'StringValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, StringValue) and o.value == self.value


class StructValue(Value):
    def __init__(self, struct_type, fields: dict):
        self.struct_type = struct_type
        self.fields = fields

    def get_type(self):
        return self.struct_type

    def access(self, field):
        return self.fields[field]

    def __repr__(self):
        return f'StructValue({repr(self.struct_type)}, {repr(self.fields)})'

    def __eq__(self, o):
        return (
            isinstance(o, StructValue) and
            o.struct_type == self.struct_type and
            o.fields == self.fields
        )


class LambdaValue(Value):
    def __init__(self, scope, lambda_type, arg_names, body):
        self.scope = scope
        self.lambda_type = lambda_type
        self.arg_names = arg_names
        self.body = body

    def get_type(self):
        return self.lambda_type

    def get_eval_scope(self, arg_values):
        return self.scope.make_child(self.arg_names, arg_values)

    def __repr__(self):
        return f'LambdaValue({repr(self.scope)}, {repr(self.lambda_type)}, {repr(self.arg_names)}, {repr(self.body)})'

    def __eq__(self, o):
        # Doesn't compare scopes
        return (
            isinstance(o, LambdaValue) and
            o.lambda_type == self.lambda_type and
            o.arg_names == self.arg_names and
            o.body == self.body
        )


class FunctionValue(Value):
    def __init__(self, declaration):
        self.declaration = declaration

    def get_type(self):
        return self.declaration.get_type()

    def __repr__(self):
        return f'FunctionValue({repr(self.declaration)})'

    def __eq__(self, o):
        return isinstance(o, FunctionValue) and o.declaration == self.declaration


class BuiltinFunction(Value):
    def __init__(self, name):
        self.name = name

    def get_type(self):
        return None

    def __repr__(self):
        return f'BuiltinFunction({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, BuiltinFunction) and o.name == self.name
