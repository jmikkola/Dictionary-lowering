# module syntax

from interpreter.types import Type, TClass, TypeVariable, Qualified, TConstructor

class Literal:
    def get_type(self) -> Type:
        raise NotImplementedError


class LString(Literal):
    def __init__(self, value: str):
        self.value = value

    def __str__(self):
        # Let python's repr escape the string for us
        # (but, it uses single quotes instead of double quotes, so strip those
        # off and replace them)
        escaped = repr(self.value)[1:-1]
        return '"' + escaped + '"'

    def __repr__(self):
        return f'LString({repr(self.value)})'

    def get_type(self) -> Type:
        return TConstructor('String')


class LInt(Literal):
    def __init__(self, value: int):
        self.value = value

    def __str__(self):
        return f'{self.value}'

    def __repr__(self):
        return f'LInt({repr(self.value)})'

    def get_type(self) -> Type:
        return TConstructor('Int')


class LFloat(Literal):
    def __init__(self, value: float):
        self.value = value

    def __str__(self):
        return f'{self.value}'

    def __repr__(self):
        return f'LFloat({repr(self.value)})'

    def get_type(self) -> Type:
        return TConstructor('Float')


class Expression:
    def get_type(self) -> Type:
        raise NotImplementedError


class ELiteral(Expression):
    def __init__(self, literal: Literal):
        self.literal = literal

    def __str__(self):
        return str(self.literal)

    def __repr__(self):
        return f'ELiteral({repr(self.literal)})'

    def get_type(self) -> Type:
        return self.literal.get_type()


class EVariable(Expression):
    def __init__(self, t: Type, name: str):
        self.t = t
        self.name = name

    def __str__(self):
        return name

    def __repr__(self):
        return f'EVariable({repr(self.name)})'

    def get_type(self) -> Type:
        return self.t


class ECall(Expression):
    def __init__(self, t: Type, f_expr: Expression, arg_exprs: list):
        self.t = t
        self.f_expr = f_expr
        self.arg_exprs = arg_exprs

    def __str__(self):
        joined_args = ' '.join(str(a) for a in self.arg_exprs)
        if joined_args:
            joined_args = ' ' + joined_args
        return f'({self.f_expr}{joined_args})'

    def __repr__(self):
        return f'ECall({repr(self.f_expr)}, {repr(self.arg_exprs)})'

    def get_type(self) -> Type:
        return self.t


class EPartial(Expression):
    ''' Like a function call, except for partial application.
    Not exposed as part of the syntax currently. '''

    def __init__(self, t: Type, f_expr: Expression, arg_exprs: list):
        self.t = t
        self.f_expr = f_expr
        self.arg_exprs = arg_exprs

    def __str__(self):
        joined_args = ' '.join(str(a) for a in self.arg_exprs)
        if joined_args:
            joined_args = ' ' + joined_args
        return f'(*apply* {self.f_expr}{joined_args})'

    def __repr__(self):
        return f'EPartial({repr(self.f_expr)}, {repr(self.arg_exprs)})'

    def get_type(self) -> Type:
        return self.t


class EParen(Expression):
    def __init__(self, inner: Expression):
        self.inner = inner

    def __str__(self):
        return '(' + str(self.inner) + ')'

    def __repr__(self):
        return f'EParen({repr(self.inner)})'

    def get_type(self) -> Type:
        return self.inner.get_type()


class Binding:
    def __init__(self, name: str, value: Expression):
        self.name = name
        self.value = value

    def __str__(self):
        return f'{self.name} = {self.expression}'

    def __repr__(self):
        return f'Binding({repr(self.name)}, {repr(self.value)})'


class ELet(Expression):
    def __init__(self, t: Type, bindings: list, inner: Expression):
        self.t = t
        self.bindings = bindings
        self.inner = inner

    def __str__(self):
        binds = ', '.join(str(b) for b in self.bindings)
        return f'let {binds} in {self.inner}'

    def __repr__(self):
        return f'ELet({repr(self.bindings)}, {repr(self.inner)})'

    def get_type(self) -> Type:
        return self.t


class ELambda(Expression):
    def __init__(self, t: Type, arg_names: list, body: Expression):
        self.t = t
        self.arg_names = arg_names
        self.body = body

    def __str__(self):
        args = ' '.join(self.arg_names)
        return f'(\{args} -> {self.body})'

    def __repr__(self):
        return f'ELambda({repr(self.arg_names)}, {repr(self.body)})'

    def get_type(self) -> Type:
        return self.t


class Declaration:
    pass


class DFunction(Declaration):
    def __init__(self, name: str, t, arg_names: list, body: Expression):
        self.name = name
        self.t = t
        self.arg_names = arg_names
        self.body = body

    def __str__(self):
        args = ' '.join(self.arg_names)
        return f'{self.name} :: {self.t}\n{self.name} {args} = {self.body}'

    def __repr__(self):
        return f'DFunction({repr(self.name)}, {repr(self.t)}, {repr(self.arg_names)}, {repr(self.body)})'


class MethodDecl:
    def __init__(self, method_name: str, qual_type: Qualified):
        self.method_name = method_name
        self.qual_type = qual_type

    def __str__(self):
        return f'{self.method_name} :: {self.qual_type}'

    def __repr__(self):
        return f'MethodDecl({repr(self.method_name)}, {repr(self.qual_type)})'

    def get_type(self):
        return self.qual_type.t


class ClassDef:
    def __init__(self, tclass: TClass, supers: list, tvar: TypeVariable, methods: list):
        self.tclass = tclass
        self.supers = supers
        self.tvar = tvar
        self.methods = methods

    def __str__(self):
        super_part = ''
        if len(self.supers) > 0:
            super_part = ', '.join(f'{sup} {self.tvar}' for sup in self.supers)
            super_part += ' => '

        result = f'class {super_part}{self.tclass} {self.tvar} where'
        for method in self.methods:
            result += '\n  ' + str(method)

        return result

    def __repr__(self):
        return f'ClassDef({repr(self.tclass)}, {repr(self.supers)}, {repr(self.tvar)}, {repr(self.methods)})'

    def get_method(self, name):
        for method in methods:
            if method.name == name:
                return method
        raise RuntimeError(f'could not find method by name: {name}')


class InstanceDef:
    def __init__(self, qual_pred: Qualified, method_impls: list):
        self.qual_pred = qual_pred
        self.method_impls = method_impls

    def __str__(self):
        result = f'instance {self.qual_pred} where'
        for method in method_impls:
            result += '\n' + add_indent(str(method))
        return result

    def __repr__(self):
        return f'InstanceDef({repr(self.qual_pred)}, {repr(self.method_impls)})'

    def get_class(self):
        pred = self.qual_pred.t
        return pred.tclass

    def get_type(self):
        pred = self.qual_pred.t
        return pred.t

    def get_predicates(self):
        return self.qual_pred.predicates


def add_indent(text):
    indent = '  '
    return '\n'.join(indent + line for line in text.split('\n'))
