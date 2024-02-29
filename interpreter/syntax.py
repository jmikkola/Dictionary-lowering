# module syntax

import typing

from interpreter import types
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
        # Now double quotes need to be escaped
        escaped = escaped.replace('"', '\\"')
        return '"' + escaped + '"'

    def __repr__(self):
        return f'LString({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, LString) and o.value == self.value

    def to_lisp(self):
        return str(self)

    def get_type(self) -> Type:
        return TConstructor('String')


class LInt(Literal):
    def __init__(self, value: int):
        self.value = value

    def __str__(self):
        return f'{self.value}'

    def __repr__(self):
        return f'LInt({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, LInt) and o.value == self.value

    def to_lisp(self):
        return str(self)

    def get_type(self) -> Type:
        return TConstructor('Int')


class LFloat(Literal):
    def __init__(self, value: float):
        self.value = value

    def __str__(self):
        return f'{self.value}'

    def __repr__(self):
        return f'LFloat({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, LFloat) and o.value == self.value

    def to_lisp(self):
        return str(self)

    def get_type(self) -> Type:
        return TConstructor('Float')


class LBool(Literal):
    def __init__(self, value: bool):
        self.value = value

    def __str__(self):
        return f'{self.value}'

    def __repr__(self):
        return f'LBool({repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, LBool) and o.value == self.value

    def to_lisp(self):
        return 'true' if self.value else 'false'

    def get_type(self) -> Type:
        return TConstructor('Bool')


class Expression:
    def __init__(self, t):
        self.t = t

    def get_type(self) -> Type:
        raise NotImplementedError


class ELiteral(Expression):
    def __init__(self, literal: Literal):
        self.literal = literal

    def __str__(self):
        return str(self.literal)

    def __repr__(self):
        return f'ELiteral({repr(self.literal)})'

    def __eq__(self, o):
        return isinstance(o, ELiteral) and o.literal == self.literal

    def to_lisp(self):
        return self.literal.to_lisp()

    def get_type(self) -> Type:
        return self.literal.get_type()


class EVariable(Expression):
    def __init__(self, t: Type, name: str):
        self.t = t
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'EVariable({repr(self.t)}, {repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, EVariable) and o.t == self.t and o.name == self.name

    def to_lisp(self):
        lisp = self.name
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class ECall(Expression):
    ''' for calling a function '''

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
        return f'ECall({repr(self.t)}, {repr(self.f_expr)}, {repr(self.arg_exprs)})'

    def __eq__(self, o):
        return (
            isinstance(o, ECall) and
            o.t == self.t and
            o.f_expr == self.f_expr and
            o.arg_exprs == self.arg_exprs
        )

    def to_lisp(self):
        lisp = [self.f_expr.to_lisp()]
        lisp += [a.to_lisp() for a in self.arg_exprs]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class EConstruct(Expression):
    ''' for constructing an instance of a struct '''

    def __init__(self, t: Type, struct_name: str, arg_exprs: list):
        self.t = t
        assert(struct_name[0].isupper())
        self.struct_name = struct_name
        self.arg_exprs = arg_exprs

    def __str__(self):
        joined_args = ' '.join(str(a) for a in self.arg_exprs)
        if joined_args:
            joined_args = ' ' + joined_args
        return f'({self.struct_name}{joined_args})'

    def __repr__(self):
        return f'EConstruct({repr(self.t)}, {repr(self.struct_name)}, {repr(self.arg_exprs)})'

    def __eq__(self, o):
        return (
            isinstance(o, EConstruct) and
            o.t == self.t and
            o.struct_name == self.struct_name and
            o.arg_exprs == self.arg_exprs
        )

    def to_lisp(self):
        lisp = ['new', self.struct_name]
        lisp += [a.to_lisp() for a in self.arg_exprs]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

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
        return f'(*partial* {self.f_expr}{joined_args})'

    def __repr__(self):
        return f'EPartial({repr(self.t)}, {repr(self.f_expr)}, {repr(self.arg_exprs)})'

    def __eq__(self, o):
        return (
            isinstance(o, EPartial) and
            o.t == self.t and
            o.f_expr == self.f_expr and
            o.arg_exprs == self.arg_exprs
        )

    def to_lisp(self):
        lisp = ['*partial*', self.f_expr.to_lisp()]
        lisp += [a.to_lisp() for a in self.arg_exprs]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class EAccess(Expression):
    ''' Access a field of a struct '''

    def __init__(self, t: Type, lhs: Expression, field: str):
        self.t = t
        self.lhs = lhs
        self.field = field

    def __str__(self):
        return f'(. {self.lhs} {self.field})'

    def __repr__(self):
        return f'EAccess({repr(self.t)}, {repr(self.lhs)}, {repr(self.field)})'

    def __eq__(self, o):
        return (
            isinstance(o, EAccess) and
            o.t == self.t and
            o.lhs == self.lhs and
            o.field == self.field
        )

    def to_lisp(self):
        lisp = ['.', self.lhs.to_lisp(), self.field]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class Binding:
    def __init__(self, name: str, value: Expression):
        self.name = name
        self.value = value

    def __str__(self):
        return f'{self.name} = {self.value}'

    def __repr__(self):
        return f'Binding({repr(self.name)}, {repr(self.value)})'

    def __eq__(self, o):
        return isinstance(o, Binding) and o.name == self.name and o.value == self.value

    def to_lisp(self):
        return [self.name, self.value.to_lisp()]


class ELet(Expression):
    def __init__(self, t: Type, bindings: list, inner: Expression):
        self.t = t
        self.bindings = bindings
        self.inner = inner

    def __str__(self):
        binds = ', '.join(str(b) for b in self.bindings)
        return f'let {binds} in {self.inner}'

    def __repr__(self):
        return f'ELet({repr(self.t)}, {repr(self.bindings)}, {repr(self.inner)})'

    def __eq__(self, o):
        return (
            isinstance(o, ELet) and
            o.t == self.t and
            o.bindings == self.bindings and
            o.inner == self.inner
        )

    def to_lisp(self):
        bindings = [b.to_lisp() for b in self.bindings]
        lisp = ['let', bindings, self.inner.to_lisp()]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class EIf(Expression):
    def __init__(self, t: Type, test: Expression, if_case: Expression, else_case: Expression):
        self.t = t
        self.test = test
        self.if_case = if_case
        self.else_case = else_case

    def __str__(self):
        return f'(if {self.test} {self.if_case} {self.else_case})'

    def __repr__(self):
        return f'EIf({repr(self.t)}, {repr(self.test)}, {repr(self.if_case)}, {repr(self.else_case)})'

    def __eq__(self, o):
        return (
            isinstance(o, EIf) and
            o.t == self.t and
            o.test == self.test and
            o.if_case == self.if_case and
            o.else_case == self.else_case
        )

    def to_lisp(self):
        lisp = ['if', self.test.to_lisp(), self.if_case.to_lisp(), self.else_case.to_lisp()]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

    def get_type(self) -> Type:
        return self.t


class ELambda(Expression):
    def __init__(self, t: Type, arg_names: list, body: Expression):
        self.t = t
        self.arg_names = arg_names
        self.body = body

    def __str__(self):
        args = ' '.join(self.arg_names)
        return f'(\ {args} -> {self.body})'

    def __repr__(self):
        return f'ELambda({repr(self.t)}, {repr(self.arg_names)}, {repr(self.body)})'

    def __eq__(self, o):
        return (
            isinstance(o, ELambda) and
            o.t == self.t and
            o.arg_names == self.arg_names and
            o.body == self.body
        )

    def to_lisp(self):
        lisp = ['\\', self.arg_names, self.body.to_lisp()]
        if self.t is not None:
            lisp = add_type(lisp, self.t)
        return lisp

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

    def __eq__(self, o):
        return (
            isinstance(o, DFunction) and
            o.name == self.name and
            o.t == self.t and
            o.arg_names == self.arg_names and
            o.body == self.body
        )

    def to_lisp(self):
        lisp = ['fn', self.name, self.arg_names]
        if self.t is not None:
            lisp.append(self.t.to_lisp())
        lisp.append(self.body.to_lisp())
        return lisp


class MethodDecl:
    def __init__(self, method_name: str, qual_type: Qualified):
        self.method_name = method_name
        self.qual_type = qual_type

    def __str__(self):
        return f'{self.method_name} :: {self.qual_type}'

    def __repr__(self):
        return f'MethodDecl({repr(self.method_name)}, {repr(self.qual_type)})'

    def __eq__(self, o):
        return (
            isinstance(o, MethodDecl) and
            o.method_name == self.method_name and
            o.qual_type == self.qual_type
        )

    def to_lisp(self):
        return ['::', self.method_name, self.qual_type.to_lisp()]

    def get_type(self):
        return self.qual_type.t

    def num_args(self):
        return types.num_args(self.get_type())


class ClassDef:
    # supers: list[TClass]
    # methods: list[MethodDecl]
    def __init__(self, tclass: TClass, supers: list, tvar: TypeVariable, methods: list):
        self.tclass = tclass
        self.supers = supers
        self.tvar = tvar
        self.methods = methods  # type: typing.List[MethodDecl]

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

    def __eq__(self, o):
        return (
            isinstance(o, ClassDef) and
            o.tclass == self.tclass and
            o.supers == self.supers and
            o.tvar == self.tvar and
            o.methods == self.methods
        )

    def get_class_predicate(self):
        return types.Predicate(self.tclass, types.TVariable(self.tvar))

    def method_names(self):
        return set(m.method_name for m in self.methods)

    def class_name(self):
        return self.tclass.name

    def to_lisp(self):
        lisp = ['class', [self.tclass.to_lisp(), self.tvar.to_lisp()]]
        if len(self.supers) > 0:
            lisp.append('superclasses')
            lisp.append([s.to_lisp() for s in self.supers])
        lisp += [m.to_lisp() for m in self.methods]
        return lisp

    def get_method(self, name):
        for method in self.methods:
            if method.method_name == name:
                return method
        raise RuntimeError(f'could not find method by name: {name}')


class InstanceDef:
    # method_name: list[DFunction]
    def __init__(self, qual_pred: Qualified, method_impls: list):
        self.qual_pred = qual_pred
        self.method_impls = method_impls  # type: typing.List[DFunction]

    def __str__(self):
        result = f'instance {self.qual_pred} where'
        for method in self.method_impls:
            result += '\n' + add_indent(str(method))
        return result

    def __repr__(self):
        return f'InstanceDef({repr(self.qual_pred)}, {repr(self.method_impls)})'

    def __eq__(self, o):
        return (
            isinstance(o, InstanceDef) and
            o.qual_pred == self.qual_pred and
            o.method_impls == self.method_impls
        )

    def to_lisp(self):
        lisp = ['instance', self.qual_pred.to_lisp()]
        lisp += [m.to_lisp() for m in self.method_impls]
        return lisp

    def get_predicate(self):
        ''' Returns a predicate containing the implemented class and the type the class is implemented for '''
        return self.qual_pred.t

    def get_class(self):
        ''' Returns the class being implemented. '''
        pred = self.qual_pred.t
        return pred.tclass

    def get_type(self):
        ''' Returns the type the class is implemented for. '''
        pred = self.qual_pred.t
        return pred.t

    def get_predicates(self):
        ''' Returns the predicates that must be satisfied for this instance to be used. '''
        return self.qual_pred.predicates


class StructDef:
    def __init__(self, name: str, type_vars: list, fields: list):
        '''
        type_vars: list[TypeVariable]
        fields: list[tuple[str, Type]]
        '''
        self.name = name
        self.type_vars = type_vars
        self.fields = fields

    def __str__(self):
        return render_lisp(self.to_lisp())

    def __repr__(self):
        return f'StructDef({repr(self.name)}, {repr(self.type_vars)}, {repr(self.fields)})'

    def __eq__(self, o):
        return (
            isinstance(o, StructDef) and
            o.name == self.name and
            o.type_vars == self.type_vars and
            o.fields == self.fields
        )

    def get_field_types(self):
        return [t for (_name, t) in self.fields]

    def find_field_type(self, name):
        for (field_name, t) in self.fields:
            if field_name == name:
                return t

    def to_lisp(self):
        if len(self.type_vars) == 0:
            name_part = self.name
        else:
            name_part = [self.name] + [str(tv) for tv in self.type_vars]
        lisp = ['struct', name_part]
        for (name, t) in self.fields:
            lisp.append(add_type(name, t))
        return lisp


def add_indent(text):
    indent = '  '
    return '\n'.join(indent + line for line in text.split('\n'))


def add_type(lisp, t):
    ''' Creates (:: value type) nodes '''
    return ['::', lisp, t.to_lisp()]


def render_lisp(lisp):
    ''' converts e.g. ['f', '1'] back into "(f 1)" '''
    if isinstance(lisp, str):
        return lisp

    inner = ' '.join(render_lisp(l) for l in lisp)
    return f'({inner})'
