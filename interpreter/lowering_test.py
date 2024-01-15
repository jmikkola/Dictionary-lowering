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

    def test_lowering_class_use_for_concrete_type(self):
        input_text = '''
          (fn show_an_int ()
            (Fn String)
            (:: ((:: show (Fn Int String)) 12345) String))

          (class (Show a)
            (:: show (Fn a String)))

          (instance (Show Int)
            (fn show (i) "TODO"))
 '''

        output_text = '''
(fn make__ShowMethods__Int ()
  (Fn (ShowMethods Int))
  (:: (new ShowMethods
        (:: (\ (i) "TODO") (Fn Int String)))
  (ShowMethods Int)))

(fn show_an_int ()
  (Fn String)
  (:: ((:: (. (:: ((:: make__ShowMethods__Int (Fn ShowMethods))) ShowMethods) show) (Fn Int String)) 12345) String))

(struct (ShowMethods a)
  (:: show (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)

    def test_pass_predicates_to_class_method(self):
        input_text = '''
(class (Foldable t)
  (:: foldl (Fn (Fn b a b) b (t a) b))
  (:: elem (=> ((Eq a)) (Fn a (t a) Bool))))

(class (Eq a)
  (:: == (Fn a a Bool)))

(fn pass_method_to_class (item list)
  (=> ((Foldable f) (Eq x)) (Fn x (f x) Bool))
  (::
    ((:: elem (Fn x (f x) Bool))
     (:: item x)
     (:: list (f x)))
    Bool))
'''

        output_text = '''
(fn pass_method_to_class (dict_Foldable_f dict_Eq_x item list)
  (Fn (FoldableMethods f) (EqMethods x) x (f x) Bool)
  (:: ((:: (. (:: dict_Foldable_f (FoldableMethods f)) elem)
           (Fn (EqMethods x) x (f x) Bool))
       (:: dict_Eq_x (EqMethods x))
       (:: item x)
       (:: list (f x)))
      Bool))

(struct (FoldableMethods t)
  (:: foldl (Fn (Fn b a b) b (t a) b))
  (:: elem (Fn (EqMethods a) a (t a) Bool)))

(struct (EqMethods a)
  (:: == (Fn a a Bool)))
'''

        self.assert_lowers(input_text, output_text)

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

    def test_class_and_instance_with_predicates_on_method(self):
        # This also tests an instance that uses other instance methods

        input_text = '''
          (class (Next n)
            (:: next (Fn n n))
            (:: showNext (=> ((Show n)) (Fn n String))))

          (class (Show a)
            (:: show (Fn a String)))

          (instance (Show Int)
            (fn show (n) "not implemented"))

          (instance (Next Int)
            (fn next (x)
              (:: ((:: inc (Fn Int Int)) (:: x Int)) Int))
            (fn showNext (x)
              (:: ((:: show (Fn Int String)) ((:: next (Fn Int Int)) (:: x Int))) String)))
'''

        output_text = '''
          (fn make__ShowMethods__Int ()
            (Fn (ShowMethods Int))
            (:: (new ShowMethods
                  (:: (\ (n) "not implemented") (Fn Int String)))
              (ShowMethods Int)))

          ;; If you think this is bad, imagine what the raw AST would look like
          (fn make__NextMethods__Int ()
            (Fn (NextMethods Int))
            (:: (new NextMethods
                  (:: (\ (x) (:: ((:: inc (Fn Int Int)) (:: x Int)) Int)) (Fn Int Int))
                  (::
                    (\ (x) (::
                             ((::
                                (. (::
                                     ((::
                                        make__ShowMethods__Int
                                        (Fn ShowMethods)))
                                     ShowMethods)
                                   show)
                                (Fn Int String))
                               ((::
                                   (. (::
                                        ((::
                                           make__NextMethods__Int
                                           (Fn NextMethods)))
                                        NextMethods)
                                      next)
                                   (Fn Int Int))
                                 (:: x Int)))
                               String))
                    (Fn Int String)))
              (NextMethods Int)))

          (struct (NextMethods n)
            (:: next (Fn n n))
            (:: showNext (Fn (ShowMethods n) n String)))

          (struct (ShowMethods a)
            (:: show (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)

    def test_instance_with_predicates(self):
        input_text = '''
          (class (Show a)
            (:: show (Fn a String)))

          (struct (Pair a)
            (:: x a)
            (:: y a))

          ;; I really need a type inference algorithm
          (instance (=> ((Show a)) (Show (Pair a)))
            (fn show (pair)
              (::
                (
                 (:: join (Fn String String String))
                 (::
                   (
                    (:: show (Fn a String))
                    (:: (. (:: pair (Pair a)) x) a))
                   String)
                 (::
                   (
                    (:: show (Fn a String))
                    (:: (. (:: pair (Pair a)) y) a))
                   String))
                String)))
'''

        output_text = '''
          ;; I don't love the Pair<a> naming scheme...
          (fn make__ShowMethods__Pair<a> (dict_Show_a)
            (Fn (ShowMethods a) (ShowMethods (Pair a)))
            (::
              (new ShowMethods
                (::
                  (\ (pair)
                    (::
                      ((:: join (Fn String String String))
                       (::
                         ((:: (. (:: dict_Show_a (ShowMethods a)) show) (Fn a String))
                          (:: (. (:: pair (Pair a)) x) a))
                         String)
                       (::
                         ((:: (. (:: dict_Show_a (ShowMethods a)) show) (Fn a String))
                          (:: (. (:: pair (Pair a)) y) a))
                         String))
                      String))
                  (Fn (Pair a) String)))
              (ShowMethods (Pair a))))

          (struct (ShowMethods a)
            (:: show (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)

    def test_using_superclass_in_instance(self):
        input_text = '''
(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c)
  superclasses (Parent)
  (:: child (Fn c Int)))

(instance (Parent String)
  (fn parent (s)
    (:: ((:: length (Fn String Int)) (:: s String)) Int)))

(instance (Child String)
  (fn child (s)
    (::
      ((:: inc (Fn Int Int))
       (:: ((:: parent (Fn String Int)) (:: s String)) Int))
      Int)))
'''

        output_text = '''
(fn make__ParentMethods__String ()
  (Fn (ParentMethods String))
  (::
    (new ParentMethods
      (::
        (\ (s)
          (:: ((:: length (Fn String Int)) (:: s String)) Int))
        (Fn String Int)))
    (ParentMethods String)))

(fn make__ChildMethods__String ()
  (Fn (ChildMethods String))
  (::
    (new ChildMethods
      (::
        ((:: make__ParentMethods__String (Fn ParentMethods)))
        ParentMethods)
      (::
        (\ (s)
          (::
            ((:: inc (Fn Int Int))
             (::
               ((::
                  (. (:: ((:: make__ParentMethods__String (Fn ParentMethods))) ParentMethods) parent)
                  (Fn String Int))
                (:: s String))
               Int))
            Int))
        (Fn String Int)))
    (ChildMethods String)))

(struct (ParentMethods p)
  (:: parent (Fn p Int)))

(struct (ChildMethods c)
  (:: superParent (ParentMethods c))
  (:: child (Fn c Int)))
'''

        self.assert_lowers(input_text, output_text)

    # TODO: Test lowering code with other expression types:
    # - access
    # - if statements
    # - let bindings


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
