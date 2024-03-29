# module program

import typing

from interpreter import format
from interpreter import syntax


class Program:
    ''' Program holds the contents of a program.

    This is used to communicate between compiler stages in a uniform way.
    '''

    # from_stage: str - which compiler stage this has passed through already
    # functions: list[syntax.DFunction]
    # structs: list[syntax.StructDef]
    # classes: list[syntax.ClassDef]
    # instances: list[syntax.InstanceDef]
    def __init__(self, from_stage, functions, structs, classes, instances):
        self.from_stage = from_stage  # type: str
        self.functions = functions  # type: typing.List[syntax.DFunction]
        self.structs = structs  # type: typing.List[syntax.StructDef]
        self.classes = classes  # type: typing.List[syntax.ClassDef]
        self.instances = instances # type: typing.List[syntax.InstanceDef]

    def to_lisp(self, show_builtins=False):
        lisp = [f.to_lisp() for f in self.functions if show_builtins or not f.is_builtin]
        lisp += [s.to_lisp() for s in self.structs if show_builtins or not s.is_builtin]
        lisp += [c.to_lisp() for c in self.classes if show_builtins or not c.is_builtin]
        lisp += [i.to_lisp() for i in self.instances if show_builtins or not i.is_builtin]
        return lisp

    def format(self, show_builtins=False):
        lines = [
            format.format_declaration(decl)
            for decl in self.to_lisp(show_builtins)
        ]
        return '\n\n'.join(lines)

    def __eq__(self, o):
        return (
            isinstance(o, Program) and
            o.from_stage == self.from_stage and
            o.functions == self.functions and
            o.structs == self.structs and
            o.classes == self.classes and
            o.instances == self.instances
        )

    def __str__(self):
        lines = []
        lines.extend(str(f) for f in self.functions)
        lines.extend(str(s) for s in self.structs)
        lines.extend(str(c) for c in self.classes)
        lines.extend(str(i) for i in self.instances)
        return '\n'.join(lines)

    def __repr__(self):
        stage = repr(self.from_stage)
        f = repr(self.functions)
        s = repr(self.structs)
        c = repr(self.classes)
        i = repr(self.instances)
        return f'Program({stage}, {f}, {s}, {c}, {i})'

    def get_class(self, name: str):
        for c in self.classes:
            if c.class_name() == name:
                return c
        return None

    def get_function(self, name: str):
        for f in self.functions:
            if f.name == name:
                return f
        return None
