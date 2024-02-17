# module types

class TypeVariable:
    def __init__(self, name: str):
        assert(isinstance(name, str))
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'TypeVariable({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, TypeVariable) and o.name == self.name

    def __hash__(self):
        return hash(('TypeVariable', self.name))

    def to_lisp(self):
        return str(self)


class Type:
    pass


class TVariable(Type):
    def __init__(self, type_variable: TypeVariable):
        assert(isinstance(type_variable, TypeVariable))
        self.type_variable = type_variable

    def __str__(self):
        return str(self.type_variable)

    def __repr__(self):
        return f'TVariable({repr(self.type_variable)})'

    def __eq__(self, o):
        return isinstance(o, TVariable) and o.type_variable == self.type_variable

    def to_lisp(self):
        return str(self)

    def free_type_vars(self):
        return set([self.type_variable])

    @classmethod
    def from_varname(cls, name):
        return TVariable(TypeVariable(name))

    def apply(self, substitution):
        replacement = substitution.get(self.type_variable)
        if replacement is not None:
            return replacement
        return self


class TConstructor(Type):
    def __init__(self, type_name: str):
        self.type_name = type_name

    def __str__(self):
        return self.type_name

    def __repr__(self):
        return f'TConstructor({repr(self.type_name)})'

    def __eq__(self, o):
        return isinstance(o, TConstructor) and o.type_name == self.type_name

    def to_lisp(self):
        return str(self)

    def free_type_vars(self):
        return set()

    def apply(self, substitution):
        return self


class TApplication(Type):
    def __init__(self, t, args: list):
        self.t = t
        assert(isinstance(args, list))
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

    def to_lisp(self):
        lisp = [self.t.to_lisp()]
        lisp += [a.to_lisp() for a in self.args]
        return lisp

    def free_type_vars(self):
        ftvs = self.t.free_type_vars()
        for a in self.args:
            ftvs |= a.free_type_vars()
        return ftvs

    def apply(self, substitution):
        return TApplication(
            self.t.apply(substitution),
            substitution.apply_to_list(self.args)
        )


class TClass:
    def __init__(self, name: str):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'TClass({repr(self.name)})'

    def __eq__(self, o):
        return isinstance(o, TClass) and o.name == self.name

    def to_lisp(self):
        return str(self)


class Predicate:
    def __init__(self, tclass: TClass, t: Type):
        self.tclass = tclass
        self.t = t

    def __str__(self):
        return f'({self.tclass} {self.t})'

    def __repr__(self):
        return f'Predicate({repr(self.tclass)}, {repr(self.t)})'

    def __eq__(self, o):
        return isinstance(o, Predicate) and o.tclass == self.tclass and o.t == self.t

    def to_lisp(self):
        return [self.tclass.to_lisp(), self.t.to_lisp()]

    def apply(self, substitution):
        return Predicate(
            self.tclass,
            self.t.apply(substitution)
        )


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

    def to_lisp(self):
        predicates = [p.to_lisp() for p in self.predicates]
        return ['=>', predicates, self.t.to_lisp()]

    def apply(self, substitution):
        return Qualified(
            substitution.apply_to_list(self.predicates),
            self.t.apply(substitution)
        )

    def unqualify(self):
        return self.t


class Substitution:
    # dict[TyVar, Type]
    def __init__(self, substitutions: dict):
        self.substitutions = substitutions

    def __eq__(self, o):
        return isinstance(o, Substitution) and o.substitutions == self.substitutions

    def __repr__(self):
        return f'Substitution({repr(self.substitutions)})'

    def __contains__(self, key):
        return key in self.substitutions

    def __getitem__(self, key):
        return self.substitutions[key]

    @classmethod
    def singleton(cls, type_variable: TypeVariable, replacement: Type):
        return Substitution({type_variable: replacement})

    @classmethod
    def empty(cls):
        return Substitution({})

    # other: Substitution
    def merge(self, other):
        # Check if the two substitutions agree before merging
        for (tvar, t) in self.substitutions.items():
            t2 = other.get(tvar)
            if t2 is not None and t2 != t:
                raise TypeError(f'substitutions do not agree: {self.substitutions} and {other}')

        merged = {**self.substitutions, **other.substitutions}
        return Substitution(merged)

    def get(self, type_variable: TypeVariable):
        return self.substitutions.get(type_variable)

    def apply_to_list(self, items: list):
        return [item.apply(self) for item in items]


class TypeError(RuntimeError):
    pass


def can_types_unify(l: Type, r: Type) -> bool:
    try:
        most_general_unifier(l, r)
        return True
    except TypeError:
        return False


def most_general_unifier(l: Type, r: Type) -> Substitution:
    # TODO: an actual implementation
    return Substitution.empty()


def match(l: Type, r: Type) -> Substitution:
    '''
    Returns a substitution that, if applied to `l`, will result in `r`.

    Raises a TypeError if matching the types fails
    '''

    if isinstance(l, TApplication) and isinstance(r, TApplication):
        if len(l.args) == len(r.args):
            sub = match(l.t, r.t)
            for (l_arg, r_arg) in zip(l.args, r.args):
                sub = sub.merge(match(l_arg, r_arg))
            return sub
    elif isinstance(l, TVariable):
        return Substitution.singleton(l.type_variable, r)
    elif isinstance(l, TConstructor) and isinstance(r, TConstructor):
        if l == r:
            return Substitution.empty()

    raise TypeError(f'types do not match: {l} and {r}')


def make_function_type(arg_types: list, return_type: Type) -> Type:
    return TApplication(
        TConstructor('Fn'),
        arg_types + [return_type]
    )


def require_function_type(t: Type):
    ''' asserts T is a function type, then returns (args, return) '''
    assert(isinstance(t, TApplication))
    assert(t.t == TConstructor('Fn'))
    return (t.args[:-1], t.args[-1])
