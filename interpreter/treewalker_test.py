# module treewalker test

import unittest

from interpreter import lowering
from interpreter import parser
from interpreter import treewalker


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

        result = eval_expression('(fib 10)', file_text=text, use_lowering=False)

        expected = treewalker.IntValue(55)
        self.assertEqual(expected, result)

    def test_calling_class_instance_method(self):
        text = '''
          (fn call_show_pair ()
            (Fn String)
            (:: ((:: show_pair (Fn (Pair Int) String))
                     (:: (new Pair 123 456) (Pair Int)))
                String))

          (fn show_pair (pair)
             (=> ((Show t)) (Fn (Pair t) String))
             (:: ((:: concat (Fn String String String))
                  (:: ((:: show (Fn t String))
                       (:: (. (:: pair (Pair t)) x) t))
                      String)
                  (:: ((:: concat (Fn String String String))
                       ", "
                       (:: ((:: show (Fn t String))
                            (:: (. (:: pair (Pair t)) y) t))
                           String))
                      String))
                 String))

          (struct (Pair a)
            (:: x a)
            (:: y a))

          (class (Show s)
            (:: show (Fn s String)))

          (instance (Show Int)
            (fn show (i)
              ;; use the built-in str function
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))
'''

        result = eval_expression('(call_show_pair)', file_text=text)

        expected = treewalker.StringValue('123, 456')
        self.assertEqual(expected, result)


def eval_expression(text, file_text=None, use_lowering=True):
    expression = parse_expression(text)
    intp = treewalker.Interpreter()

    if file_text is not None:
        parsed = parser.parse(file_text)
        if use_lowering:
            lowering_input = lowering.LoweringInput(
                parsed.functions,
                parsed.structs,
                parsed.classes,
                parsed.instances
            )
            lowering_output = lowering_input.lower()
            intp.load_declarations(lowering_output.declarations)
            intp.load_structs(lowering_output.dictionaries)
        else:
            intp.load_declarations(parsed.functions)
            intp.load_structs(parsed.structs)

    return intp._eval_expression('<test>', treewalker.Scope(), expression)


def parse_expression(text):
    sexpr = parser._parse_lists(text)[0]
    return parser._parse_expression(sexpr)


if __name__ == '__main__':
    unittest.main()
