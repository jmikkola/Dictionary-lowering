# module treewalker test

import unittest

from interpreter import builtin
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
  (:: (if (:: ((:: <:Int (Fn Int Int Bool)) (:: x Int) 2) Bool)
        (:: x Int)
        (:: ((:: +:Int (Fn Int Int))
             (:: ((:: fib (Fn Int Int))
                  (:: ((:: -:Int (Fn Int Int)) (:: x Int) 1) Int))
                 Int)
             (:: ((:: fib (Fn Int Int))
                  (:: ((:: -:Int (Fn Int Int)) (:: x Int) 2) Int))
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
          (fn call_show__pair ()
            (Fn String)
            (:: ((:: show__pair (Fn (Pair Int) String))
                     (:: (new Pair 123 456) (Pair Int)))
                String))

          (fn show__pair (pair)
             (=> ((Show_ t)) (Fn (Pair t) String))
             (:: ((:: concat (Fn String String String))
                  (:: ((:: show_ (Fn t String))
                       (:: (. (:: pair (Pair t)) x) t))
                      String)
                  (:: ((:: concat (Fn String String String))
                       ", "
                       (:: ((:: show_ (Fn t String))
                            (:: (. (:: pair (Pair t)) y) t))
                           String))
                      String))
                 String))

          (struct (Pair a)
            (:: x a)
            (:: y a))

          (class (Show_ s)
            (:: show_ (Fn s String)))

          (instance (Show_ Int)
            (fn show_ (i)
              ;; use the built-in str function
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))
'''

        result = eval_expression('(call_show__pair)', file_text=text)

        expected = treewalker.StringValue('123, 456')
        self.assertEqual(expected, result)

    def test_return_an_instance_method(self):
        text = '''
          (fn use_instance_method ()
            (Fn String)
            (:: ((:: ((:: get_instance_method (Fn (Fn Int String))))
                     (Fn Int String))
                 123)
                String))

          (fn get_instance_method ()
            (Fn (Fn Int String))
            ;; return the `show_` function for Int
            (:: show_ (Fn Int String)))

          (class (Show_ s)
            (:: show_ (Fn s String)))

          (instance (Show_ Int)
            (fn show_ (i)
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))
'''

        result = eval_expression('(use_instance_method)', file_text=text)

        expected = treewalker.StringValue('123')
        self.assertEqual(expected, result)

    def test_return_partially_applied_function(self):
        text = '''
          (fn use_partially_applied_function ()
            (Fn String)
            (:: ((:: ((:: get_partially_applied_function
                         (Fn (Fn Int String))))
                     (Fn Int String))
                 321)
                String))

          (fn get_partially_applied_function ()
            (Fn (Fn Int String))
            (:: show__thing (Fn Int String)))

          (fn show__thing (thing)
            (=> ((Show_ a)) (Fn a String))
            (:: ((:: show_ (Fn a String)) (:: thing a)) String))

          (class (Show_ s)
            (:: show_ (Fn s String)))

          (instance (Show_ Int)
            (fn show_ (i)
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))
'''

        result = eval_expression('(use_partially_applied_function)', file_text=text)

        expected = treewalker.StringValue('321')
        self.assertEqual(expected, result)

    def test_let_expressions(self):
        input_text = '''
          (fn tenth-power (x)
             (Fn Int Int)
             (let ((x2  (*:Int x x))
                   (x4 (*:Int x2 x2))
                   (x8 (*:Int x4 x4)))
                (*:Int x8 x2)))
'''

        result = eval_expression('(tenth-power 2)', file_text=input_text)

        expected = treewalker.IntValue(1024)
        self.assertEqual(expected, result)

    def test_recursive_functions_in_let_expressions(self):
        ''' Demonstrate that let bindings can be recursive or even mutually recursive '''

        input_text = '''
          (fn collatz-seq (n)
            (Fn Int String)
            (let ((add-num (\ (n rest)
                            (concat (str n) (concat "," rest))))
                  (odd (\ (n) (ctz (+:Int (*:Int n 3) 1))))
                  (even (\ (n) (ctz (/:Int n 2))))
                  (ctz (\ (n)
                         (if (==:Int n 1)
                           "1"
                           (add-num n
                                  (if (==:Int (% n 2) 0) (even n) (odd n)))))))
                (ctz n)))
'''

        result = eval_expression('(collatz-seq 12)', file_text=input_text)

        expected = treewalker.StringValue("12,6,3,10,5,16,8,4,2,1")
        self.assertEqual(expected, result)

    def test_plus_with_different_types(self):
        cases = [
            ('(+ 1 2)', treewalker.IntValue(3)),
            ('(+ 1.0 2.0)', treewalker.FloatValue(3.0)),
            ('(+ "1" "2")', treewalker.StringValue("12")),
        ]

        for (expression, expected) in cases:
            result = eval_expression(expression)
            self.assertEqual(expected, result, expression)

    def test_integer_operations(self):
        cases = [
            ('(+ 10 12)', 22),
            ('(* 15 2)', 30),
            ('(% 15 2)', 1),
            ('(% 15 3)', 0),
            ('(/ 16 2)', 8),
            ('(- 16 1)', 15),
        ]

        for (expression, int_val) in cases:
            result = eval_expression(expression)
            expected = treewalker.IntValue(int_val)
            self.assertEqual(expected, result, expression)

    def test_builtins(self):
        ''' Ensure all built-in functions are implemented '''

        val1 = treewalker.IntValue(1)
        val2 = treewalker.IntValue(2)
        val3 = treewalker.IntValue(3)
        val5 = treewalker.IntValue(5)
        val15 = treewalker.IntValue(15)
        valTrue = treewalker.BoolValue(True)
        valFalse = treewalker.BoolValue(False)
        valFoo = treewalker.StringValue("foo")
        valBar = treewalker.StringValue("bar")
        valFoobar = treewalker.StringValue("foobar")
        val5str = treewalker.StringValue('5')
        valVoid = treewalker.VoidValue()

        cases = {
            '!=': ([val1, val1], valFalse),
            '%': ([val15, val2], val1),
            '*': ([val1, val2], val2),
            '+': ([val1, val2], val3),
            '-': ([val3, val1], val2),
            '/': ([val15, val3], val5),
            '<': ([val5, val15], valTrue),
            '>': ([val5, val15], valFalse),
            '<=': ([val5, val5], valTrue),
            '>=': ([val5, val5], valTrue),
            '==': ([val5, val5], valTrue),
            'and': ([valTrue, valFalse], valFalse),
            'or': ([valTrue, valFalse], valTrue),
            'concat': ([valFoo, valBar], valFoobar),
            'inc': ([val2], val3),
            'length': ([valFoo], val3),
            'not': ([valFalse], valTrue),
            'print': ([valFoo], valVoid),
            'str': ([val5], val5str),
        }

        print_fn = lambda s: None
        intp = treewalker.Interpreter(print_fn=print_fn)

        for name in builtin.NAMES:
            self.assertIn(name, cases)
            (args, expected) = cases[name]
            result = intp._call_builtin(name, args)
            self.assertEqual(expected, result, name)


def eval_expression(text, file_text=None, use_lowering=True):
    expression = parse_expression(text)
    intp = treewalker.Interpreter()

    if file_text is not None:
        parsed = parser.parse(file_text)
        if use_lowering:
            lowering_input = lowering.LoweringInput(parsed)
            lowering_output = lowering_input.lower()
            intp.load_program(lowering_output)
        else:
            intp.load_declarations(parsed.functions)
            intp.load_structs(parsed.structs)

    return intp._eval_expression('<test>', treewalker.Scope(), expression)


def parse_expression(text):
    sexpr = parser._parse_lists(text)[0]
    return parser._parse_expression(sexpr)


if __name__ == '__main__':
    unittest.main()
