# module treewalker

'''
This is a tree-walking interpreter
'''


from interpreter import builtin
from interpreter import syntax
from interpreter import types
from interpreter.program import Program


def interpret(program: Program):
    intp = Interpreter()
    intp.load_program(program)
    intp.eval_main()


class InterpreterError(RuntimeError):
    pass


class NameError(InterpreterError):
    pass


class TypeError(InterpreterError):
    pass


class Interpreter:
    def __init__(self, print_fn=None):
        self.print_fn = print_fn

        self.declarations = {}
        self.structs = {}

        self.builtin = set(builtin.NAMES)

    def load_program(self, program):
        assert(isinstance(program, Program))
        assert(program.classes == [])
        assert(program.instances == [])
        self.load_structs(program.structs)
        self.load_declarations(program.functions)

    def load_structs(self, structs):
        ''' Add struct definitions to the interpreter '''
        for struct in structs:
            self.structs[struct.name] = struct

    def load_declarations(self, declarations):
        ''' Add function definitions to the interpreter '''
        for decl in declarations:
            self.declarations[decl.name] = decl

    def eval_main(self):
        ''' Start running the main function '''
        self._call_function('main', [])

    def _call_function(self, name, arg_values):
        declaration = self._get_declaration(name)
        scope = Scope.for_args(declaration.arg_names, arg_values)
        return self._eval_expression(name, scope, declaration.body)

    def _eval_expression(self, function_name, scope, expression):
        ''' This is the main evaluation function '''

        if isinstance(expression, syntax.EVariable):
            name = expression.name
            if name in self.builtin:
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

            # Unwrap partial application[s]
            while isinstance(function, PartialApValue):
                arg_values = function.arg_values + arg_values
                function = function.function_value

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

            struct_value = StructValue(expression.get_type(), {
                field[0]: value
                for (field, value) in zip(struct_decl.fields, arg_values)
            })
            return struct_value
        elif isinstance(expression, syntax.EPartial):
            function_value = self._eval_expression(function_name, scope, expression.f_expr)
            arg_values = [
                self._eval_expression(function_name, scope, arg_expr)
                for arg_expr in expression.arg_exprs
            ]

            return PartialApValue(expression.get_type(), function_value, arg_values)
        elif isinstance(expression, syntax.EAccess):
            lhs = self._eval_expression(function_name, scope, expression.lhs)
            assert(isinstance(lhs, StructValue))
            return lhs.access(expression.field)
        elif isinstance(expression, syntax.ELet):
            let_scope = Scope(parent=scope)
            # Bind all variables in the same scope.  This allows bindings to
            # refer to themselves so they can be recursive functions.
            for binding in expression.bindings:
                # Start values off as None so that the Scope can exist prior to
                # computing the values
                let_scope.add_variable(binding.name, None)

            # Actually evaluate the bound expressions, in order
            for binding in expression.bindings:
                value = self._eval_expression(function_name, let_scope, binding.value)
                let_scope.update(binding.name, value)

            # Finally, evaluate the body that uses the new scope
            return self._eval_expression(function_name, let_scope, expression.inner)
        elif isinstance(expression, syntax.EIf):
            test_value = self._eval_expression(function_name, scope, expression.test)
            assert(isinstance(test_value, BoolValue))
            if test_value.value:
                return self._eval_expression(function_name, scope, expression.if_case)
            else:
                return self._eval_expression(function_name, scope, expression.else_case)
        elif isinstance(expression, syntax.ELambda):
            return LambdaValue(scope, expression.get_type(), expression.arg_names, expression.body)

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
            arg_type = self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue, StringValue))
            result_value = arg_values[0].value + arg_values[1].value
            return arg_type(result_value)
        elif name == '-':
            arg_type = self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue))
            result_value = arg_values[0].value - arg_values[1].value
            return arg_type(result_value)
        elif name == '*':
            arg_type = self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue))
            result_value = arg_values[0].value * arg_values[1].value
            return arg_type(result_value)
        elif name == '/':
            arg_type = self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue))
            result_value = arg_values[0].value / arg_values[1].value
            if arg_type == IntValue:
                result_value = int(result_value)
            return arg_type(result_value)
        elif name == '%':
            arg_type = self._expect_arg_types(name, arg_values, 2, (IntValue,))
            result_value = arg_values[0].value % arg_values[1].value
            return arg_type(result_value)
        elif name == '<':
            self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue, StringValue))
            result_value = arg_values[0].value < arg_values[1].value
            return BoolValue(result_value)
        elif name == '>':
            self._expect_arg_types(name, arg_values, 2, (IntValue, FloatValue, StringValue))
            result_value = arg_values[0].value > arg_values[1].value
            return BoolValue(result_value)
        elif name == '<=':
            value_types = (IntValue, FloatValue, StringValue)
            self._expect_arg_types(name, arg_values, 2, value_types)
            result_value = arg_values[0].value <= arg_values[1].value
            return BoolValue(result_value)
        elif name == '>=':
            value_types = (IntValue, FloatValue, StringValue)
            self._expect_arg_types(name, arg_values, 2, value_types)
            result_value = arg_values[0].value >= arg_values[1].value
            return BoolValue(result_value)
        elif name == '==':
            value_types = (IntValue, FloatValue, StringValue, BoolValue, StructValue)
            self._expect_arg_types(name, arg_values, 2, value_types)
            result_value = arg_values[0] == arg_values[1]
            return BoolValue(result_value)
        elif name == '!=':
            value_types = (IntValue, FloatValue, StringValue, BoolValue, StructValue)
            self._expect_arg_types(name, arg_values, 2, value_types)
            result_value = arg_values[0] != arg_values[1]
            return BoolValue(result_value)
        elif name == 'str':
            assert(len(arg_values) == 1)
            s = arg_values[0].builtin_str()
            return StringValue(s)
        elif name == 'concat':
            self._expect_arg_types(name, arg_values, 2, (StringValue,))
            return StringValue(arg_values[0].value + arg_values[1].value)
        elif name == 'print':
            assert(len(arg_values) == 1)
            assert(isinstance(arg_values[0], StringValue))
            if self.print_fn is not None:
                self.print_fn(arg_values[0].value)
            else:
                print(arg_values[0].value)
            return VoidValue()
        elif name == 'and':
            self._expect_arg_types(name, arg_values, 2, (BoolValue,))
            result_value = arg_values[0].value and arg_values[1].value
            return BoolValue(result_value)
        elif name == 'or':
            self._expect_arg_types(name, arg_values, 2, (BoolValue,))
            result_value = arg_values[0].value or arg_values[1].value
            return BoolValue(result_value)
        elif name == 'not':
            self._expect_arg_types(name, arg_values, 1, (BoolValue,))
            result_value = not arg_values[0].value
            return BoolValue(result_value)
        elif name == 'inc':
            self._expect_arg_types(name, arg_values, 1, (IntValue,))
            result_value = arg_values[0].value + 1
            return IntValue(result_value)
        elif name == 'length':
            self._expect_arg_types(name, arg_values, 1, (StringValue,))
            result_value = len(arg_values[0].value)
            return IntValue(result_value)

        raise NotImplementedError('builtin function not implemented: ' + name)

    def _expect_arg_types(self, name, arg_values, valid_arg_len, valid_types):
        ''' Returns the actual type of the arg used '''

        if len(arg_values) != valid_arg_len:
            raise TypeError(f'Expected {valid_arg_len} args for {name}, got {len(arg_values)}')

        arg_type = type(arg_values[0])
        if arg_type not in valid_types:
            ts = ', '.join(t.__name__ for t in valid_types)
            at = arg_type.__name__
            raise TypeError(f'Expected args for {name} to be of type {ts}, got {at}')

        for arg in arg_values[1:]:
            if type(arg) != arg_type:
                ta = type(arg).__name__
                at = arg_type.__name__
                raise TypeError(f'Mismatched arg types for {name}: {at} and {ta}')

        return arg_type


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

    def builtin_str(self):
        return str(self.value)


class FloatValue(Value):
    def __init__(self, value: float):
        self.value = value

    def get_type(self):
        return types.TConstructor('Float')

    def __repr__(self):
        return f'FloatValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, FloatValue) and o.value == self.value

    def builtin_str(self):
        return str(self.value)


class IntValue(Value):
    def __init__(self, value: int):
        self.value = value

    def get_type(self):
        return types.TConstructor('Int')

    def __repr__(self):
        return f'IntValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, IntValue) and o.value == self.value

    def builtin_str(self):
        return str(self.value)


class StringValue(Value):
    def __init__(self, value: str):
        self.value = value

    def get_type(self):
        return types.TConstructor('String')

    def __repr__(self):
        return f'StringValue({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, StringValue) and o.value == self.value

    def builtin_str(self):
        return self.value


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

    def builtin_str(self):
        header = str(self.struct_type) + '{'
        body = ', '.join(
            f'{field}={value.builtin_str()}'
            for (field, value) in self.fields.items()
        )
        return header + body + '}'


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

    def builtin_str(self):
        return 'Lambda(' + ', '.join(self.arg_names) + ')'


class FunctionValue(Value):
    def __init__(self, declaration):
        self.declaration = declaration

    def get_type(self):
        return self.declaration.get_type()

    def __repr__(self):
        return f'FunctionValue({repr(self.declaration)})'

    def __eq__(self, o):
        return isinstance(o, FunctionValue) and o.declaration == self.declaration

    def builtin_str(self):
        return f'Function({self.declaration.name})'


class BuiltinFunction(Value):
    def __init__(self, name):
        self.name = name

    def get_type(self):
        return None

    def __repr__(self):
        return f'BuiltinFunction({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, BuiltinFunction) and o.name == self.name

    def builtin_str(self):
        return f'Builtin({self.name})'

class PartialApValue(Value):
    def __init__(self, expr_type, function_value, arg_values):
        self.expr_type = expr_type
        self.function_value = function_value
        self.arg_values = arg_values

    def get_type(self):
        return self.expr_type

    def __repr__(self):
        return f'PartialApValue({repr(self.expr_type)}, {repr(self.function_value)}, {repr(self.arg_values)})'

    def __eq__(self, o):
        return (
            isinstance(o, PartialApValue) and
            o.expr_type == self.expr_type and
            o.function_value == self.function_value and
            o.arg_values == self.arg_values
        )

    def builtin_str(self):
        f_str = self.function_value.builtin_str()
        return f'PartiallyApplied({f_str})'

class VoidValue(Value):
    def get_type(self):
        return types.TConstructor('Void')

    def __repr__(self):
        return 'VoidValue()'

    def __eq__(self, o):
        return isinstance(o, VoidValue)

    def builtin_str(self):
        return 'Void'
