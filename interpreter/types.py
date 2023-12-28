# module types

class TypeVariable:
    def __init__(self, name: str):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'TypeVariable({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, TypeVariable) and o.name == self.name


class Type:
    pass


class TVariable(Type):
    def __init__(self, type_variable: TypeVariable):
        self.type_variable = type_variable

    def __str__(self):
        return self.type_variable

    def __repr__(self):
        return f'TVariable({repr(self.type_variable)})'

    def __eq__(self, o):
        return isinstance(o, TVariable) and o.type_variable == self.type_variable

    def free_type_vars(self):
        return set([self.type_variable])

    @classmethod
    def from_varname(cls, name):
        return TVariable(TypeVariable(name))


class TConstructor(Type):
    def __init__(self, type_name: str):
        self.type_name = type_name

    def __str__(self):
        return self.type_name

    def __repr__(self):
        return f'TConstructor({repr(self.type_name)})'

    def free_type_vars(self):
        return set()

    def __eq__(self, o):
        return isinstance(o, TConstructor) and o.type_name == self.type_name


class TApplication(Type):
    def __init__(self, t, args):
        self.t = t
        self.args = args

    def __str__(self):
        if len(self.args) == 0:
            return self.t

        args_joined = ', '.join(str(a) for a in self.args)
        return f'{self.t}<{args_joined}>'

    def __repr__(self):
        return f'TApplication({repr(self.t)}, {repr(self.args)})'

    def __eq__(self, o):
        return isinstance(o, TApplication) and o.t == self.t and o.args == self.args

    def free_type_vars(self):
        ftvs = self.t.free_type_vars()
        for a in args:
            ftvs |= a.free_type_vars()
        return ftvs


class TClass:
    def __init__(self, name: str):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'TClass({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, TClass) and o.name == self.name


class Predicate:
    def __init__(self, tclass: TClass, t: Type):
        self.tclass = tclass
        self.t = t

    def __str__(self):
        return f'({self.tclass} {self.f})'

    def __repr__(self):
        return f'Predicate({repr(self.tclass)}, {repr(self.t)})'

    def __eq__(self, o):
        return isinstance(o, Predicate) and o.tclass == self.tclass and o.t == self.t


class Qualified:
    # This version of ubuntu is too old to get python 3.9+, but the types should be
    # def __init__(self, predicates: list[Predicate], t: Type | Predicate):
    def __init__(self, predicates, t):
        self.predicates = predicates
        self.t = t

    def __str__(self):
        if len(self.predicates) == 0:
            return str(self.t)

        joined_preds = ', '.join(str(p) for p in self.predicates)
        return f'{joined_preds} => {self.t}'

    def __repr__(self):
        return f'Qualified({repr(self.predicates)}, {repr(self.t)})'

    def __eq__(self, o):
        return isinstance(o, Qualified) and o.predicates == self.predicates and o.t == self.t
