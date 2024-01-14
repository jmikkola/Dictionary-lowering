import unittest

from interpreter import lowering
from interpreter import parser
from interpreter import syntax
from interpreter import types

'''
Example of pretty-printing the results:

        for lisp in result.to_lisp():
            print(syntax.render_lisp(lisp))
'''


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

    def test_lowers_class_with_superclasses(self):
        text = '''
(class (MyClass a)
    superclasses (Ord Show)
    (:: doThings (Fn a a)))
'''

        lowering_input = make_lowering_input(text)
        result = lowering_input.lower()

        super_ord_type = types.TApplication(
            types.TConstructor("OrdMethods"),
            [types.TVariable.from_varname('a')]
        )
        super_show_type = types.TApplication(
            types.TConstructor("ShowMethods"),
            [types.TVariable.from_varname('a')]
        )
        do_things_type = types.make_function_type(
            [types.TVariable.from_varname('a')],
            types.TVariable.from_varname('a')
        )
        my_class_dictionary = lowering.Dictionary(
            'MyClassMethods',
            ['a'],
            [
                ("superOrd", super_ord_type),
                ("superShow", super_show_type),
                ('doThings', do_things_type),
            ]
        )
        expected = lowering.LoweringOutput(
            declarations=[],
            dictionaries=[my_class_dictionary]
        )

        self.assertEqual(expected, result)

    def test_lowers_class_with_predicates_on_method(self):
        text = '''
      (class (Foldable t)
        (:: foldl (Fn (Fn b a b) b (t a) b))
        (:: elem (=> ((Eq a)) (Fn a (t a) Bool))))
'''

        lowering_input = make_lowering_input(text)
        result = lowering_input.lower()

        a = types.TVariable.from_varname('a')
        b = types.TVariable.from_varname('b')
        t = types.TVariable.from_varname('t')

        foldl_args = [
            types.make_function_type([b, a], b),
            b,
            types.TApplication(t, [a]),
        ]
        foldl_type = types.make_function_type(foldl_args, b)

        elem_args = [
            # First arg is to pass the Eq dictionary
            types.TApplication(types.TConstructor('EqMethods'), [a]),
            a,
            types.TApplication(t, [a]),
        ]
        bool = types.TConstructor('Bool')
        elem_type = types.make_function_type(elem_args, bool)

        foldable_dictionary = lowering.Dictionary(
            'FoldableMethods',
            ['t'],
            [
                ('foldl', foldl_type),
                ('elem', elem_type),
            ]
        )
        expected = lowering.LoweringOutput(
            declarations=[],
            dictionaries=[foldable_dictionary]
        )

        self.assertEqual(expected, result)

    def test_lowering_simple_function(self):
        text = '''
      (fn some_fn (a)
         (Fn Int Int)
         (::
            ((:: some_fn (Fn Int Int))
             (:: a Int))
          Int))
'''
        # I didn't say the function would work!

        lowering_input = make_lowering_input(text)
        result = lowering_input.lower()

        some_fn = lowering_input.declarations[0]

        # The output is almost the same, just with an unqualified type
        updated_fn = syntax.DFunction(
            name=some_fn.name,
            t=some_fn.t.unqualify(),
            arg_names=some_fn.arg_names,
            body=some_fn.body,
        )

        expected = lowering.LoweringOutput(
            declarations=[updated_fn],
            dictionaries=[]
        )

        self.assertEqual(expected, result)

    # TODO: Test lowering functions
    # - adding arguments to functions
    # - looking up dictionaries in arguments
    # - looking up dictionaries that are the parents of arguments
    # - looking up instances for concrete types

    # TODO: Test creating instance functions
    # - basic test case
    # - one where a method has additional predicates
    # - one where an instance has predicates (that are used in the definition)
    # - one where a method references the superclass of the current instance's class


def print_diff(expected, result):
    print(syntax.render_lisp(expected.to_lisp()))
    print('---')
    print(syntax.render_lisp(result.to_lisp()))


def make_lowering_input(text):
    parse_result = parser.parse(text)

    return lowering.LoweringInput(
        declarations=parse_result.functions,
        classes=parse_result.classes,
        instances=parse_result.instances,
    )

if __name__ == '__main__':
    unittest.main()
