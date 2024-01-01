import unittest

from interpreter import syntax
from interpreter import types
from interpreter.parser import (
    _parse_class_definition,
    _parse_expression,
    _parse_function_declaration,
    _parse_instance_definition,
    _parse_lists,
    _parse_qualified_type,
    _parse_type,
)


class ParserTest(unittest.TestCase):
    def test_parses_lists(self):
        result = _parse_lists('() (:x (+ 1 2) "asdf") ;; comment\n (.)')
        expected = [[], [':x', ['+', '1', '2'], '"asdf"'], ['.']]
        self.assertEqual(expected, result)

    def test_parse_int(self):
        result = _parse_expression('123')
        expected = syntax.ELiteral(syntax.LInt(123))
        self.assertEqual(expected, result)

    def test_parse_float(self):
        result = _parse_expression('45.1e2')
        expected = syntax.ELiteral(syntax.LFloat(45.1e2))
        self.assertEqual(expected, result)

    def test_parses_string(self):
        result = _parse_expression('"foo\\n\\""')
        expected = syntax.ELiteral(syntax.LString('foo\n"'))
        self.assertEqual(expected, result)

    def test_parses_variable(self):
        result = _parse_expression('+add')
        expected = syntax.EVariable(None, '+add')
        self.assertEqual(expected, result)

    def test_parses_let_binding(self):
        result = _parse_expression(_parse_lists('(let ((x 123) (y 456)) x)')[0])
        expected = syntax.ELet(
            None,
            [
             syntax.Binding('x', syntax.ELiteral(syntax.LInt(123))),
             syntax.Binding('y', syntax.ELiteral(syntax.LInt(456))),
            ],
            syntax.EVariable(None, 'x')
        )
        self.assertEqual(expected, result)

    def test_parses_type_variable(self):
        result = _parse_type(_parse_lists('a')[0])
        expected = types.TVariable.from_varname('a')
        self.assertEqual(expected, result)

    def test_parses_type(self):
        result = _parse_type(_parse_lists('(List (Pair a a))')[0])
        expected = types.TApplication(
            types.TConstructor('List'),
            [
                types.TApplication(
                    types.TConstructor('Pair'),
                    [
                        types.TVariable.from_varname('a'),
                        types.TVariable.from_varname('a'),
                    ]
                )
             ]
        )
        self.assertEqual(expected, result)

    def test_parses_typed_expression(self):
        result = _parse_expression(_parse_lists('(:: x String)')[0])
        expected = syntax.EVariable(types.TConstructor('String'), 'x')
        self.assertEqual(expected, result)

    def test_parses_if_expression(self):
        result = _parse_expression(_parse_lists('(if x y z)')[0])
        expected = syntax.EIf(
            None,
            syntax.EVariable(None, 'x'),
            syntax.EVariable(None, 'y'),
            syntax.EVariable(None, 'z'),
        )
        self.assertEqual(expected, result)

    def test_parses_new_expression(self):
        result = _parse_expression(_parse_lists('(new Link value next)')[0])
        expected = syntax.EConstruct(None, 'Link', [
            syntax.EVariable(None, 'value'),
            syntax.EVariable(None, 'next'),
        ])
        self.assertEqual(expected, result)

    def test_parses_access(self):
        result = _parse_expression(_parse_lists('(. some_struct some_field)')[0])
        expected = syntax.EAccess(
            None,
            syntax.EVariable(None, 'some_struct'),
            'some_field'
        )
        self.assertEqual(expected, result)

    def test_parses_lambda(self):
        result = _parse_expression(_parse_lists('(\ (a b) a)')[0])
        expected = syntax.ELambda(
            None,
            ['a', 'b'],
            syntax.EVariable(None, 'a')
        )
        self.assertEqual(expected, result)

    def test_parses_call_no_args(self):
        result = _parse_expression(_parse_lists('(exit)')[0])
        expected = syntax.ECall(None, syntax.EVariable(None, 'exit'), [])
        self.assertEqual(expected, result)

    def test_parses_call_with_args(self):
        result = _parse_expression(_parse_lists('(+ 1 2)')[0])
        expected = syntax.ECall(
            None,
            syntax.EVariable(None, '+'),
            [
                syntax.ELiteral(syntax.LInt(1)),
                syntax.ELiteral(syntax.LInt(2)),
            ]
        )
        self.assertEqual(expected, result)

    def test_parses_predicated_type(self):
        text = '(=> ((Show a) (Eq a)) (List a))'
        result = _parse_qualified_type(_parse_lists(text)[0])
        expected = types.Qualified(
            [
                types.Predicate(
                    types.TClass('Show'),
                    types.TVariable.from_varname('a')
                ),
                types.Predicate(
                    types.TClass('Eq'),
                    types.TVariable.from_varname('a')
                ),
            ],
            types.TApplication(
                types.TConstructor('List'),
                [types.TVariable.from_varname('a')]
            )
        )
        self.assertEqual(expected, result)

    def test_parses_function_declaration(self):
        text = '''
(fn times_two (x)
  (Fn Int Int)
  (* 2 x))
'''
        result = _parse_function_declaration(_parse_lists(text)[0])
        expected = syntax.DFunction(
            'times_two',
            types.Qualified(
                [],
                types.TApplication(
                    types.TConstructor('Fn'),
                    [types.TConstructor('Int'), types.TConstructor('Int')]
                )
            ),
            ['x'],
            syntax.ECall(
                None,
                syntax.EVariable(None, '*'),
                [
                    syntax.ELiteral(syntax.LInt(2)),
                    syntax.EVariable(None, 'x'),
                ]
            )
        )
        self.assertEqual(expected, result)

    def test_parses_realistic_function(self):
        ''' This function example is only realistic while this doesn't
        have type inference, so the source text must be fully annotated with types '''

        text = '''
(fn times_two (x)
  (Fn Int Int)
  (::
    ((:: * (Fn Int Int Int)) 2 (:: x Int))
    Int))
'''
        result = _parse_function_declaration(_parse_lists(text)[0])

        Fn = types.TConstructor('Fn')
        Int = types.TConstructor('Int')

        expected = syntax.DFunction(
            'times_two',
            types.Qualified(
                [],
                types.TApplication(Fn, [Int, Int])
            ),
            ['x'],
            syntax.ECall(
                Int,
                syntax.EVariable(
                    types.TApplication(Fn, [Int, Int, Int]),
                    '*'
                ),
                [
                    syntax.ELiteral(syntax.LInt(2)),
                    syntax.EVariable(Int, 'x'),
                ]
            )
        )
        self.assertEqual(expected, result)

    def test_parses_class_definition(self):
        text = '''
      (class (Foldable t)
        (:: foldl (Fn (Fn b a b) b (t a) b))
        (:: elem (=> ((Eq a)) (Fn a (t a) Bool))))
'''
        result = _parse_class_definition(_parse_lists(text)[0])

        a = types.TVariable.from_varname('a')
        b = types.TVariable.from_varname('b')
        t = types.TVariable.from_varname('t')

        expected = syntax.ClassDef(
            types.TClass('Foldable'),
            [],
            t.type_variable,
            [
                syntax.MethodDecl(
                    'foldl',
                    syntax.Qualified(
                        [],
                        types.make_function_type(
                            [
                                types.make_function_type([b, a], b),
                                 b,
                                 types.TApplication(t, [a]),
                            ],
                            b
                        )
                    )
                ),
                syntax.MethodDecl(
                    'elem',
                    syntax.Qualified(
                        [types.Predicate(types.TClass('Eq'), a)],
                        types.make_function_type(
                            [a, types.TApplication(t, [a])],
                            types.TConstructor('Bool')
                        )
                    )
                ),
            ]
        )

        self.assertEqual(expected, result)

    def test_parses_class_definition_with_supers(self):
        text = '''
      (class (Ord a) superclasses (Eq)
        (:: < (Fn a a Bool)))
'''
        result = _parse_class_definition(_parse_lists(text)[0])

        a = types.TVariable.from_varname('a')
        expected = syntax.ClassDef(
            types.TClass('Ord'),
            ['Eq'],
            a.type_variable,
            [
                syntax.MethodDecl(
                    '<',
                    syntax.Qualified(
                        [],
                        types.make_function_type(
                            [a, a],
                            types.TConstructor('Bool')
                        )
                    )
                )
            ]
        )

        self.assertEqual(expected, result)


    def test_parses_instance_definition(self):
        text = '''
            (instance (=> ((Show a)) (Show (List a)))
                (fn show (x) (concat "[" "]")))
'''
        result = _parse_instance_definition(_parse_lists(text)[0])

        a = types.TVariable.from_varname('a')
        Show = types.TClass('Show')
        List = types.TConstructor('List')
        expected = syntax.InstanceDef(
            types.Qualified(
                [types.Predicate(Show, a)],
                types.Predicate(
                    Show,
                    types.TApplication(List, [a])
                )
            ),
            [
                syntax.DFunction(
                    'show',
                    None,
                    ['x'],
                    syntax.ECall(
                        None,
                        syntax.EVariable(None, 'concat'),
                        [
                            syntax.ELiteral(syntax.LString("[")),
                            syntax.ELiteral(syntax.LString("]")),
                        ]
                    )
                ),
            ]
        )

        self.assertEqual(expected, result)


if __name__ == '__main__':
    unittest.main()
