# module treewalker test

import unittest

from interpreter import treewalker
from interpreter import parser


class TreewalkerTest(unittest.TestCase):
    def test_evals_ints(self):
        result = eval_expression('123')
        expected = treewalker.IntValue(123)
        self.assertEqual(expected, result)

    def test_adds_ints(self):
        result = eval_expression('(+ 3 100)')
        expected = treewalker.IntValue(103)
        self.assertEqual(expected, result)


def eval_expression(text):
    expression = parse_expression(text)
    intp = treewalker.Interpreter()
    return intp._eval_expression('<test>', treewalker.Scope(), expression)


def parse_expression(text):
    sexpr = parser._parse_lists(text)[0]
    return parser._parse_expression(sexpr)


if __name__ == '__main__':
    unittest.main()
