import unittest

from interpreter import format


class FormatTest(unittest.TestCase):
    def test_reformat_function(self):
        input_text = '''
(fn fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
'''
        output_text = '''
(fn fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
'''
        self.assert_reformats(input_text, output_text)

    def test_reformats_struct(self):
        input_text = '''
(struct Point (:: x Int) (:: y Int))
'''
        output_text = '''
(struct Point
  (:: x Int)
  (:: y Int))
'''
        self.assert_reformats(input_text, output_text)

    def test_reformats_class(self):
        input_text = '''
(class (Child c) superclasses (Parent) (:: child (Fn c Int)))
'''
        output_text = '''
(class (Child c) superclasses (Parent)
  (:: child (Fn c Int)))
'''
        self.assert_reformats(input_text, output_text)

    def test_reformat_instance(self):
        input_text = '''
(instance (=> ((Show a)) (Show (List a))) (fn show (x) (concat "[" (join " " (map show x)) "]")))
'''
        output_text = '''
(instance (=> ((Show a)) (Show (List a)))
  (fn show (x)
    (concat "[" (join " " (map show x)) "]")))
'''
        self.assert_reformats(input_text, output_text)

    def test_wraps_long_lines(self):
        input_text = '''
(fn make__ParentMethods__String () (Fn (ParentMethods
String)) (:: (new ParentMethods (:: (\ (s) (:: ((:: length
(Fn String Int)) (:: s String)) Int)) (Fn String Int))) (ParentMethods
String)))
'''
        output_text = '''
(fn make__ParentMethods__String ()
  (Fn (ParentMethods String))
  (:: (new ParentMethods
        (:: (\ (s)
              (:: ((:: length (Fn String Int))
                   (:: s String))
                  Int))
            (Fn String Int)))
      (ParentMethods String)))
'''
        self.assert_reformats(input_text, output_text)

    def assert_reformats(self, input_text, output_text):
        self.assertEqual(output_text.strip(), format.reformat(input_text))
