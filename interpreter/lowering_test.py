import unittest

from interpreter import lowering
from interpreter import parser
from interpreter import syntax
from interpreter import types


class TestLowering(unittest.TestCase):
    def test_lowers_simple_class_definition(self):
        input_text = '''
(class (Show s)
  (:: show (Fn s String)))
'''

        output_text = '''(struct (ShowMethods s) (:: show (Fn s String)))'''

        self.assert_lowers(input_text, output_text)

    def test_lowers_class_with_superclasses(self):
        input_text = '''
(class (MyClass a)
    superclasses (Ord Show)
    (:: doThings (Fn a a)))
'''

        output_text = '''
          (struct (MyClassMethods a)
            (:: superOrd (OrdMethods a))
            (:: superShow (ShowMethods a))
            (:: doThings (Fn a a)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowers_class_with_predicates_on_method(self):
        input_text = '''
      (class (Foldable t)
        (:: foldl (Fn (Fn b a b) b (t a) b))
        (:: elem (=> ((Eq a)) (Fn a (t a) Bool))))
'''

        output_text = '''
          (struct (FoldableMethods t)
            (:: foldl (Fn (Fn b a b) b (t a) b))
            (:: elem (Fn (EqMethods a) a (t a) Bool)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_simple_function(self):
        input_text = '''
      (fn some_fn (a)
         (Fn Int Int)
         (::
            ((:: some_fn (Fn Int Int))
             (:: a Int))
          Int))
'''
        # I didn't say the function would work!

        output_text = '''
            (fn some_fn (a)
              (Fn Int Int)
              (:: ((:: some_fn (Fn Int Int)) (:: a Int)) Int))
'''

        self.assert_lowers(input_text, output_text)

    def test_add_args_to_function(self):
        input_text = '''
          (fn show_a (a)
              (=> ((Show t)) (Fn t String))
           "TODO")
'''

        output_text = '''
            (fn show_a (dict_Show_t a) (Fn (ShowMethods t) t String) "TODO")
'''

        self.assert_lowers(input_text, output_text)

    def test_looks_up_dictionaries_from_arguments(self):
        # This depends on having the definition of the class to know that
        # `show` comes from it.
        input_text = '''
          (fn show_a (a)
              (=> ((Show t)) (Fn t String))
           (:: ((:: show (Fn t String)) (:: a t)) String))

          (class (Show s)
            (:: show (Fn s String)))
'''

        output_text = '''
          (fn show_a (dict_Show_t a)
            (Fn (ShowMethods t) t String)
            (:: ((:: (. (:: dict_Show_t (ShowMethods t)) show) (Fn t String)) (:: a t)) String))
          (struct (ShowMethods s)
            (:: show (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)

    def test_looks_up_parent_of_argument(self):
        input_text = '''
(fn takes_child (xc)
    (=> ((Child t)) (Fn t Int))
    (:: ((:: parent (Fn t Int)) (:: xc t)) Int))

(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c) superclasses (Parent)
  (:: child (Fn c String)))
'''

        output_text = '''
(fn takes_child (dict_Child_t xc)
  (Fn (ChildMethods t) t Int)
  (::
    ((::
      (. (:: (. (:: dict_Child_t (ChildMethods t)) superParent) (ParentMethods t)) parent)
      (Fn t Int))
     (:: xc t))
    Int))

(struct (ParentMethods p)
  (:: parent (Fn p Int)))

(struct (ChildMethods c)
  (:: superParent (ParentMethods c))
  (:: child (Fn c String)))
'''

        self.assert_lowers(input_text, output_text)

    # TODO: Test lowering functions
    # - looking up instances for concrete types


    # TODO: Test creating instance functions
    # - one where a method has additional predicates
    # - one where an instance has predicates (that are used in the definition)
    # - one where a method references the superclass of the current instance's class

    def test_simple_instance_function(self):
        input_text = '''
          (class (Show a)
            (:: show (Fn a String)))

          (instance (Show Int)
            (fn show (i)
              "TODO"))
'''

        output_text = '''
          (fn make__ShowMethods__Int ()
            (Fn (ShowMethods Int))
            (:: (new ShowMethods
                  (:: (\ (i) "TODO") (Fn Int String)))
                (ShowMethods Int)))

          (struct (ShowMethods a)
            (:: show (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)


    def assert_lowers(self, input_text, output_text):
        lowering_input = make_lowering_input(input_text)
        expected = parse_output(output_text)

        result = lowering_input.lower()

        if result != expected:
            print_diff(expected, result)

        self.assertEqual(expected, result)



def parse_output(text):
    ''' Parse a LoweringOutput.

    This allows expressing the expected output as program text.

    This should be the reverse of running print_result.
    '''
    lists = parser._parse_lists(text)

    declarations = []
    dictionaries = []

    for sexpr in lists:
        if sexpr[0] == 'fn':
            fn = parser._parse_function_declaration(sexpr)
            assert(fn.t.predicates == [])
            # Remove the predicates from the function's type
            fn.t = fn.t.unqualify()
            declarations.append(fn)
        else:
            assert(sexpr[0] == 'struct')
            struct = parser._parse_struct_definition(sexpr)
            dictionaries.append(struct)

    return lowering.LoweringOutput(
        declarations=declarations,
        dictionaries=dictionaries
    )


def print_diff(expected, result):
    print_result(expected)
    print('---')
    print_result(result)


def print_result(result):
    for lisp in result.to_lisp():
        print(syntax.render_lisp(lisp))


def make_lowering_input(text):
    parse_result = parser.parse(text)

    return lowering.LoweringInput(
        declarations=parse_result.functions,
        classes=parse_result.classes,
        instances=parse_result.instances,
    )

if __name__ == '__main__':
    unittest.main()
