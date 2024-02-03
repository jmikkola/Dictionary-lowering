# module program

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
        self.from_stage = from_stage
        self.functions = functions
        self.structs = structs
        self.classes = classes
        self.instances = instances

    def to_lisp(self):
        lisp = [f.to_lisp() for f in self.functions]
        lisp += [s.to_lisp() for s in self.structs]
        lisp += [c.to_lisp() for c in self.classes]
        lisp += [i.to_lisp() for i in self.instances]
        return lisp

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
