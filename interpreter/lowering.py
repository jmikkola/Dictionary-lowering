# module lowering


from interpreter.syntax import Declaration, ClassDef, InstanceDef
from interpreter.types import Qualified, Type


class LoweringInput:
    ''' This contains the inputs to the lowering pass '''

    def __init__(self, declarations: list, classes: list, instances: list):
        self.declarations = declarations
        self.classes = classes
        self.instances = instances


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
