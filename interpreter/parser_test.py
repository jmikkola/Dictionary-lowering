import unittest

from interpreter import syntax
from interpreter import types
from interpreter.parser import (
    _parse_class_definition,
    _parse_expression,
    _parse_function_declaration,
    _parse_instance_definition,
    _parse_lists,
    _parse_one_list,
    _parse_qualified_type,
    _parse_struct_definition,
    _parse_type,
    parse,
)
from interpreter import program


class ParserTest(unittest.TestCase):
    def test_parses_lists(self):
        result = _parse_lists('() (:x (+ 1 2) "asdf") ;; comment\n (.)')
        expected = [[], [':x', ['+', '1', '2'], '"asdf"'], ['.']]
        self.assertEqual(expected, result)

    def test_parse_int(self):
        result = _parse_expression('123')
        expected = syntax.ELiteral(syntax.LInt(123))
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parse_float(self):
        result = _parse_expression('45.1e2')
        expected = syntax.ELiteral(syntax.LFloat(45.1e2))
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_string(self):
        result = _parse_expression('"foo\\n\\""')
        expected = syntax.ELiteral(syntax.LString('foo\n"'))
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_bool(self):
        cases = [('true', True), ('false', False)]
        for (text, b) in cases:
            result = _parse_expression(text)
            expected = syntax.ELiteral(syntax.LBool(b))
            self.assertEqual(result, expected, text)
            self.roundtrip_expression(expected)

    def test_parses_variable(self):
        result = _parse_expression('+add')
        expected = syntax.EVariable(None, '+add')
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

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
        self.roundtrip_expression(expected)

    def test_parses_typed_let_binding(self):
        text = '''
(let (((:: f (=> ((Show a)) (Fn a String))) (\ (x) (show x))))
  (f 123))
'''
        result = _parse_expression(_parse_one_list(text))

        expected = syntax.ELet(
            None,
            [
                syntax.Binding(
                    'f',
                    syntax.ELambda(
                        None,
                        ['x'],
                        syntax.ECall(
                            None,
                            syntax.EVariable(None, 'show'),
                            [syntax.EVariable(None, 'x')]
                        )
                    ),
                    t=types.Qualified(
                        [
                            types.Predicate(
                                types.TClass('Show'),
                                types.TVariable.from_varname('a')
                            )
                        ],
                        types.TApplication(
                            types.TConstructor('Fn'),
                            [
                                types.TVariable.from_varname('a'),
                                types.TConstructor('String')
                            ]
                        )
                    )
                )
            ],
            syntax.ECall(
                None,
                syntax.EVariable(None, 'f'),
                [syntax.ELiteral(syntax.LInt(123))]
            )
        )

        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_type_variable(self):
        result = _parse_type(_parse_lists('a')[0])
        expected = types.TVariable.from_varname('a')
        self.assertEqual(expected, result)
        self.roundtrip_type(expected)

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
        self.roundtrip_type(expected)

    def test_parses_typed_expression(self):
        result = _parse_expression(_parse_lists('(:: x String)')[0])
        expected = syntax.EVariable(types.TConstructor('String'), 'x')
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_if_expression(self):
        result = _parse_expression(_parse_lists('(if x y z)')[0])
        expected = syntax.EIf(
            None,
            syntax.EVariable(None, 'x'),
            syntax.EVariable(None, 'y'),
            syntax.EVariable(None, 'z'),
        )
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_new_expression(self):
        result = _parse_expression(_parse_lists('(new Link value next)')[0])
        expected = syntax.EConstruct(None, 'Link', [
            syntax.EVariable(None, 'value'),
            syntax.EVariable(None, 'next'),
        ])
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_access(self):
        result = _parse_expression(_parse_lists('(. some_struct some_field)')[0])
        expected = syntax.EAccess(
            None,
            syntax.EVariable(None, 'some_struct'),
            'some_field'
        )
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_lambda(self):
        result = _parse_expression(_parse_lists('(\ (a b) a)')[0])
        expected = syntax.ELambda(
            None,
            ['a', 'b'],
            syntax.EVariable(None, 'a')
        )
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

    def test_parses_call_no_args(self):
        result = _parse_expression(_parse_lists('(exit)')[0])
        expected = syntax.ECall(None, syntax.EVariable(None, 'exit'), [])
        self.assertEqual(expected, result)
        self.roundtrip_expression(expected)

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
        self.roundtrip_expression(expected)

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
        self._roundtrip(expected, _parse_qualified_type)

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
        self._roundtrip(expected, _parse_function_declaration)

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
        self._roundtrip(expected, _parse_function_declaration)

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
        self._roundtrip(expected, _parse_class_definition)

    def test_parses_class_definition_with_supers(self):
        text = '''
      (class (Ord a) superclasses (Eq)
        (:: < (Fn a a Bool)))
'''
        result = _parse_class_definition(_parse_lists(text)[0])

        a = types.TVariable.from_varname('a')
        expected = syntax.ClassDef(
            types.TClass('Ord'),
            [types.TClass('Eq')],
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
        self._roundtrip(expected, _parse_class_definition)


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
        self._roundtrip(expected, _parse_instance_definition)

    def test_parses_simple_struct(self):
        text = '''(struct Name (:: first String) (:: last String))'''

        result = _parse_struct_definition(_parse_lists(text)[0])

        t_string = types.TConstructor('String')
        fields = [
            ('first', t_string),
            ('last', t_string),
        ]
        expected = syntax.StructDef('Name', [], fields)

        self.assertEqual(expected, result)
        self._roundtrip(expected, _parse_struct_definition)

    def test_parses_struct_with_type_variables(self):
        text = '''
            (struct (Reversible a b)
              (:: to (Fn a b))
              (:: from (Fn b a)))
'''

        result = _parse_struct_definition(_parse_lists(text)[0])

        a = types.TVariable.from_varname('a')
        b = types.TVariable.from_varname('b')
        tvs = [a.type_variable, b.type_variable]
        fields = [
            ('to', types.make_function_type([a], b)),
            ('from', types.make_function_type([b], a)),
        ]
        expected = syntax.StructDef('Reversible', tvs, fields)

        self.assertEqual(expected, result)
        self._roundtrip(expected, _parse_struct_definition)


    def test_parse(self):
        text = '''
(class (Steppable a)
  (:: step (Fn a a)))

(instance (Steppable Int)
  (fn step (x) (+ x 1)))

(fn step_all (xs)
  (=> ((Steppable a)) (Fn (List a) (List a)))
  (map step xs))

(struct (Pair a)
  (:: x a)
  (:: y a))
'''
        result = parse(text)

        a = types.TVariable.from_varname('a')
        List = types.TConstructor('List')
        List_a = types.TApplication(List, [a])

        step_all = syntax.DFunction(
            'step_all',
            types.Qualified(
                [types.Predicate(types.TClass('Steppable'), a)],
                types.make_function_type([List_a], List_a)
            ),
            ['xs'],
            syntax.ECall(
                None,
                syntax.EVariable(None, 'map'),
                [
                    syntax.EVariable(None, 'step'),
                    syntax.EVariable(None, 'xs')
                ]
            )
        )

        steppable = syntax.ClassDef(
            types.TClass('Steppable'),
            [],
            a.type_variable,
            [
                syntax.MethodDecl(
                    'step',
                    syntax.Qualified(
                        [],
                        types.make_function_type([a], a)
                    )
                ),
            ]
        )

        step_int = syntax.InstanceDef(
            types.Qualified(
                [],
                types.Predicate(
                    types.TClass('Steppable'),
                    types.TConstructor('Int')
                )
            ),
            [
                syntax.DFunction(
                    'step',
                    None,
                    ['x'],
                    syntax.ECall(
                        None,
                        syntax.EVariable(None, '+'),
                        [
                            syntax.EVariable(None, 'x'),
                            syntax.ELiteral(syntax.LInt(1)),
                        ]
                    )
                ),
            ]
        )

        pair_struct = syntax.StructDef(
            'Pair',
            [types.TypeVariable('a')],
            [
                ('x', types.TVariable.from_varname('a')),
                ('y', types.TVariable.from_varname('a')),
            ]
        )

        expected = program.Program(
            from_stage='parser',
            functions=[step_all],
            structs=[pair_struct],
            classes=[steppable],
            instances=[step_int],
        )

        self.assertEqual(expected, result)


    def roundtrip_expression(self, expression):
        self._roundtrip(expression, _parse_expression)

    def roundtrip_type(self, t):
        self._roundtrip(t, _parse_type)

    def _roundtrip(self, value, parser):
        lisp = value.to_lisp()
        parsed_from_lisp = parser(lisp)
        self.assertEqual(value, parsed_from_lisp)

        text = syntax.render_lisp(lisp)
        parsed_lisp = _parse_lists(text)[0]
        self.assertEqual(lisp, parsed_lisp)

        parsed_from_text = parser(parsed_lisp)
        self.assertEqual(value, parsed_from_text)


if __name__ == '__main__':
    unittest.main()
