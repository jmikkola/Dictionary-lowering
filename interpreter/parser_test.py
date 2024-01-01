import unittest

from interpreter import syntax
from interpreter.parser import (
    _parse_lists,
    _parse_expression,
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


if __name__ == '__main__':
    unittest.main()
