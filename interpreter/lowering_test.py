import unittest

from interpreter import lowering
from interpreter import parser
from interpreter import types


class TestLowering(unittest.TestCase):
    def test_lowers_simple_class_definition(self):
        text = '''
(class (Show s)
  (:: show (Fn s String)))
'''

        lowering_input = make_lowering_input(text)
        result = lowering_input.lower()

        show_type = types.make_function_type(
            [types.TVariable.from_varname('s')],
            types.TConstructor('String')
        )
        show_dictionary = lowering.Dictionary(
            'ShowMethods',
            ['s'],
            [('show', show_type)]
        )
        expected = lowering.LoweringOutput(
            declarations=[],
            dictionaries=[show_dictionary]
        )

        self.assertEqual(expected, result)

    # More class cases to test:
    # - classes with superclasses (maybe multiple)
    # - classes where the methods have class predicates


def make_lowering_input(text):
    parse_result = parser.parse(text)

    return lowering.LoweringInput(
        declarations=parse_result.functions,
        classes=parse_result.classes,
        instances=parse_result.instances,
    )

if __name__ == '__main__':
    unittest.main()
