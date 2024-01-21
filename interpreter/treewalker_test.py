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

    def test_recursive_function(self):
        text = '''
(fn fib (x)
  (Fn Int Int)
  (:: (if (:: ((:: < (Fn Int Int Bool)) (:: x Int) 2) Bool)
        (:: x Int)
        (:: ((:: + (Fn Int Int))
             (:: ((:: fib (Fn Int Int))
                  (:: ((:: - (Fn Int Int)) (:: x Int) 1) Int))
                 Int)
             (:: ((:: fib (Fn Int Int))
                  (:: ((:: - (Fn Int Int)) (:: x Int) 2) Int))
                 Int))
            Int))
       Int))
'''

        result = eval_expression(
            '(:: ((:: fib (Fn Int Int)) 10) Int)',
            file_text=text
        )

        expected = treewalker.IntValue(55)
        self.assertEqual(expected, result)

    def test_recursive_function_untyped(self):
        ''' show that types don't really matter for most of this interpreter '''

        text = '''
(fn fib (x)
  (if (< x 2)
    x
    (+ (fib (- x 2)) (fib (- x 1)))))
'''

        result = eval_expression('(fib 10)', file_text=text)

        expected = treewalker.IntValue(55)
        self.assertEqual(expected, result)


def eval_expression(text, file_text=None):
    expression = parse_expression(text)
    intp = treewalker.Interpreter()

    if file_text is not None:
        parsed = parser.parse(file_text)
        intp.load_declarations(parsed.functions)
        intp.load_structs(parsed.structs)

    return intp._eval_expression('<test>', treewalker.Scope(), expression)


def parse_expression(text):
    sexpr = parser._parse_lists(text)[0]
    return parser._parse_expression(sexpr)


if __name__ == '__main__':
    unittest.main()
