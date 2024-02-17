import unittest

from interpreter import check
from interpreter import parser


class CheckTest(unittest.TestCase):
    def test_checks_valid_class(self):
        text = '''
(class (Show s)
  (:: show (Fn s String)))
'''
        self.assert_no_error(text)

    def test_redefining_builtin_class(self):
        text = '''
(class (Int s)
  (:: toint (Fn s String)))
'''
        self.assert_error(text, 'Cannot redefine the builtin type Int')

    def test_duplicate_class_definitions(self):
        text = '''
(class (Show s)
  (:: show1 (Fn s String)))
(class (Show s)
  (:: show2 (Fn s String)))
'''
        self.assert_error(text, 'Duplicate declaration name Show')

    def test_checks_class_with_duplicate_method(self):
        text = '''
(class (Show s)
  (:: show (Fn s String))
  (:: show (Fn s String)))
'''
        self.assert_error(text, 'Duplicate method show for class Show')

    def test_checks_class_with_duplicate_supers(self):
        text = '''
(class (MyClass a)
    superclasses (Ord Show Ord)
    (:: doThings (Fn a a)))
'''
        self.assert_error(text, 'Duplicate super class Ord for class MyClass')

    def test_cyclic_class_hierarchy(self):
        text = '''
(class (ClassA a) superclasses (ClassB))
(class (ClassB a) superclasses (ClassC))
(class (ClassC a) superclasses (ClassA))
'''
        self.assert_error(text, 'Class hierarchy cannot be cyclic')

    def test_class_methods_must_contain_class_type_variable(self):
        text = '''
(class (ClassA a)
  (:: foo (Fn Int (List a))))
'''
        self.assert_no_error(text)

        text = '''
(class (ClassA a)
  (:: foo (Fn Int (List b))))
'''
        self.assert_error(text, 'Method foo on class ClassA must reference the class type variable')

    def test_duplicate_method_names_between_functions(self):
        text = '''
(fn f (x) x)
(fn f (x) x)
'''
        self.assert_error(text, 'Duplicate method name f')

    def test_redefining_builtin_functions(self):
        text = '(fn print (x) x)'
        self.assert_error(text, 'Cannot redefine the builtin print')

    def test_duplicate_method_names_between_functions_and_classes(self):
        text = '''
(fn f (x) x)
(class (ClassA a)
  (:: f (Fn Int (List a))))
'''
        self.assert_error(text, 'Duplicate method name f')

    def test_duplicate_method_names_between_classes(self):
        text = '''
(class (ClassA a)
  (:: f (Fn Int (List a))))
(class (ClassB a)
  (:: f (Fn Int (List a))))
'''
        self.assert_error(text, 'Duplicate method name f')

    def test_duplicate_args_in_functions(self):
        text = '''
(fn f (a b a) a)
'''
        self.assert_error(text, 'Duplicate argument a in function f')

    def test_duplicate_args_in_instance_method(self):
        text = '''
(class (ClassA a)
  (:: foo (Fn a a Int)))
(instance (ClassA Int)
  (fn foo (x x) (* 2 x)))
'''
        self.assert_error(text, 'Duplicate argument x in function foo')

    def test_duplicate_args_in_lambda(self):
        text = '''
(fn mul (x)
  (\ (y y) (+ y (* y x))))
'''
        self.assert_error(text, 'Duplicate argument y in lambda function')

    def test_duplicate_names_in_let_binding(self):
        text = '''
(fn mul (x)
  (let ((y 1) (y 2)) (+ y (* y x))))
'''
        self.assert_error(text, 'Duplicate name y in let binding')

    def test_refers_to_undefined_variable(self):
        text = '''
(fn foo (x)
  (+ x y))
'''
        self.assert_error(text, 'Undefined variable y')

    def test_can_refer_to_built_in(self):
        text = '''
(fn foo (x)
  (inc x))
'''
        self.assert_no_error(text)

    def test_struct_and_class_with_same_name(self):
        text = '''
(class (ClassA a)
  (:: f (Fn Int (List a))))
(struct ClassA
  (:: foo Int))
'''
        self.assert_error(text, 'Duplicate declaration name ClassA')

    def test_disallows_functions_names_starting_with_uppercase(self):
        text = '''
(fn Foo (a) a)
'''
        self.assert_error(text, 'function names must start with a lowercase letter, found Foo')

    def test_catches_duplicate_struct_fields(self):
        text = '''
(struct Bar
  (:: foo Int)
  (:: foo Int))
'''
        self.assert_error(text, 'struct Bar has multiple fields named foo')

    def assert_no_error(self, text):
        self.assertIsNone(self._get_error(text))

    def assert_error(self, text, message):
        error = self._get_error(text)
        self.assertIsNotNone(error)
        self.assertIn(message, str(error))

    def _get_error(self, text):
        parsed = parser.parse(text)
        try:
            check.check(parsed)
        except check.CheckFailure as e:
            return e
        return None
