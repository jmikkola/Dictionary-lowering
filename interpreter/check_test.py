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

    def test_duplicate_class_definitions(self):
        text = '''
(class (Show s)
  (:: show (Fn s String)))
(class (Show s)
  (:: show (Fn s String)))
'''
        self.assert_error(text, 'Duplicate classes named Show')

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


