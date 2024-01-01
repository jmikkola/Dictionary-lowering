import unittest

from interpreter import syntax
from interpreter import types
from interpreter.parser import (
    _parse_expression,
    _parse_lists,
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


if __name__ == '__main__':
    unittest.main()
