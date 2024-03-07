import unittest

from interpreter import inference
from interpreter import lowering
from interpreter import parser
from interpreter import syntax
from interpreter.program import Program


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
           "_")
'''

        output_text = '''
            (fn show_a (dict_Show_t a) (Fn (ShowMethods t) t String) "_")
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_struct_access(self):
        input_text = '''
          (fn show__x (pair)
             (=> ((Show_ t)) (Fn (Pair t) String))
             (:: ((:: show_ (Fn t String))
                  (:: (. (:: pair (Pair t)) x) t))
                 String))

          (struct (Pair a)
            (:: x a)
            (:: y a))

          (class (Show_ s)
            (:: show_ (Fn s String)))
'''

        output_text = '''
          (fn show__x (dict_Show__t pair)
            (Fn (Show_Methods t) (Pair t) String)
            (:: ((:: (. (:: dict_Show__t (Show_Methods t)) show_)
                     (Fn t String))
                 (:: (. (:: pair (Pair t)) x) t))
                String))

          (struct (Show_Methods s)
            (:: show_ (Fn s String)))

          (struct (Pair a)
            (:: x a)
            (:: y a))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_if_expressions(self):
        input_text = '''
          (fn show__either (first? first second)
              (=> ((Show_ a)) (Fn Bool a a String))
              (:: (if (:: first? Bool)
                    (:: ((:: show_ (Fn a String)) (:: first a)) String)
                    (:: ((:: show_ (Fn a String)) (:: second a)) String))
                  String))

          (class (Show_ s)
            (:: show_ (Fn s String)))
'''

        output_text = '''
          (fn show__either (dict_Show__a first? first second)
            (Fn (Show_Methods a) Bool a a String)
            (:: (if (:: first? Bool)
                  (:: ((:: (. (:: dict_Show__a (Show_Methods a)) show_) (Fn a String))
                       (:: first a))
                      String)
                  (:: ((:: (. (:: dict_Show__a (Show_Methods a)) show_) (Fn a String))
                       (:: second a))
                      String))
              String))

          (struct (Show_Methods s)
            (:: show_ (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_let_expression(self):
        input_text = '''
          (fn tenth-power (x)
             (Fn Int Int)
             (let ((x2  (*:Int x x))
                   (x4 (*:Int x2 x2))
                   (x8 (*:Int x4 x4)))
                (*:Int x8 x2)))
'''

        output_text = '''
          (fn tenth-power (x)
             (Fn Int Int)
             (let ((x2  (*:Int x x))
                   (x4 (*:Int x2 x2))
                   (x8 (*:Int x4 x4)))
                (*:Int x8 x2)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_returned_lambda_function_using_predicates(self):
        text = '''
(fn return-lambda (unused)
  (\ (x) (show x)))
'''

        # TODO: the dictionary arg should get added to return-lambda;
        # return values of functions aren't polymorphic after they are returned

        # TODO: create a assert_lowers_with_inference helper

    def test_lowering_let_binding_with_predicates_and_multiple_instantiations(self):
        text = '''
(fn use-to-str ()
  (let ((to-str (\ (n) (show n))))
    (concat (to-str true) (to-str ""))))
'''
        # Infer types for this
        program = inference.infer_types(parser.parse(text))
        print(show_result(program))

        # Lower it
        lowered = lowering.LoweringInput(program).lower()

        output_text = '''
(fn use-to-str ()
  (=> () (Fn String))
  (:: (let ((to-str (:: (\ (dict_Show_n n)
                           (:: ((:: (. (:: dict_Show_n (ShowMethods t4)) show)
                                    (Fn t4 String))
                                (:: n t4))
                               String))
                        (Fn t4 String))))
         (:: ((:: concat (Fn String String String))
              (:: ((:: to-str (Fn (ShowMethods Bool) Bool String))
                   (:: ((:: make__ShowMethods__Bool (Fn (ShowMethods Bool))))
                       (ShowMethods Bool))
                   true)
                  String)
              (:: ((:: to-str (Fn (ShowMethods String) String String))
                   (:: ((:: make__ShowMethods__String (Fn (ShowMethods String))))
                       (ShowMethods String))
                   "")
                  String))
             String))
      String))
'''
        output = parser.parse(output_text)
        self.assert_programs_equal(expected=output, result=lowered)

    def test_lowering_lambda_expression(self):
        input_text = '''
          (fn make-adder (x)
            (Fn Int (Fn Int Int))
            (\ (n) (+:Int x n)))
'''

        output_text = '''
          (fn make-adder (x)
            (Fn Int (Fn Int Int))
            (\ (n) (+:Int x n)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_struct_construction(self):
        input_text = '''
          (fn make-pair (x y)
            (Fn a a (Pair a))
            (new Pair x y))

          (struct (Pair a)
            (:: x a)
            (:: y a))
'''

        output_text = '''
          (fn make-pair (x y)
            (Fn a a (Pair a))
            (new Pair x y))

          (struct (Pair a)
            (:: x a)
            (:: y a))
'''

        self.assert_lowers(input_text, output_text)


    def test_looks_up_dictionaries_from_arguments(self):
        # This depends on having the definition of the class to know that
        # `show_` comes from it.
        input_text = '''
          (fn show__a (a)
              (=> ((Show_ t)) (Fn t String))
           (:: ((:: show_ (Fn t String)) (:: a t)) String))

          (class (Show_ s)
            (:: show_ (Fn s String)))
'''

        output_text = '''
          (fn show__a (dict_Show__t a)
            (Fn (Show_Methods t) t String)
            (:: ((:: (. (:: dict_Show__t (Show_Methods t)) show_) (Fn t String)) (:: a t)) String))
          (struct (Show_Methods s)
            (:: show_ (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)
        # Ensure that the output would lower to the same thing. This is a way
        # to ensure that no information is lost in the lowering pass, and the
        # output is basically sane.
        self.assert_lowers(output_text, output_text)

    def test_passes_class_predicates_to_another_method(self):
        input_text = '''
          (fn first-function (x)
            (=> ((Show_ a)) (Fn a String))
            (:: ((:: concat (Fn String String String))
                 "> "
                 (:: ((:: second-function (Fn a String)) (:: x a)) String))
                String))

          (fn second-function (x)
            (=> ((Show_ a)) (Fn a String))
            (:: ((:: show_ (Fn a String)) (:: x a)) String))

          (class (Show_ s)
            (:: show_ (Fn s String)))
'''

        output_text = '''
          (fn first-function (dict_Show__a x)
            (Fn (Show_Methods a) a String)
            (:: ((:: concat (Fn String String String))
                 "> "
                 (:: ((:: second-function
                          (Fn (Show_Methods a) a String))
                      (:: dict_Show__a (Show_Methods a))
                      (:: x a))
                     String))
                String))

          (fn second-function (dict_Show__a x)
            (Fn (Show_Methods a) a String)
            (:: ((:: (. (:: dict_Show__a (Show_Methods a)) show_) (Fn a String)) (:: x a)) String))

          (struct (Show_Methods s)
            (:: show_ (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

    def test_passes_dictionary_for_concrete_type_to_generic_method(self):
        input_text = '''
          (fn first-function ()
            (Fn String)
            (:: ((:: second-function (Fn Int String)) 123) String))

          (fn second-function (x)
            (=> ((ToString a)) (Fn a String))
            (:: ((:: toString (Fn a String)) (:: x a)) String))

          (class (ToString s)
            (:: toString (Fn s String)))

          (instance (ToString Int)
            (fn toString (i) "_"))
'''

        output_text = '''
          (fn make__ToStringMethods__Int ()
            (Fn (ToStringMethods Int))
            (:: (new ToStringMethods
                  (:: (\ (i) "_")
                      (Fn Int String)))
                (ToStringMethods Int)))

          (fn first-function ()
            (Fn String)
            (:: ((:: second-function (Fn (ToStringMethods Int) Int String))
                 (:: ((:: make__ToStringMethods__Int
                          (Fn (ToStringMethods Int))))
                     (ToStringMethods Int))
                 123)
                String))

          (fn second-function (dict_ToString_a x)
            (Fn (ToStringMethods a) a String)
            (:: ((:: (. (:: dict_ToString_a (ToStringMethods a))
                        toString)
                     (Fn a String))
                 (:: x a))
                String))

          (struct (ToStringMethods s)
            (:: toString (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

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
        self.assert_lowers(output_text, output_text)

    def test_superclass_of_superclass(self):
        input_text = '''
(fn takes_grandchild (value)
  (=> ((GrandChild t)) (Fn t Int))
  (:: ((:: parent  (Fn t Int))
       (:: value t))
      Int))

(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c) superclasses (Parent)
  (:: child (Fn c String)))

(class (GrandChild gc) superclasses (Child)
  (:: grandchild (Fn gc Bool)))
'''

        output_text = '''
(fn takes_grandchild (dict_GrandChild_t value)
  (Fn (GrandChildMethods t) t Int)
  (:: ((:: (. (:: (. (:: (. (:: dict_GrandChild_t
                               (GrandChildMethods t))
                            superChild)
                         (ChildMethods t))
                     superParent)
                  (ParentMethods t))
              parent)
           (Fn t Int))
       (:: value t))
      Int))

(struct (ParentMethods p)
  (:: parent (Fn p Int)))

(struct (ChildMethods c)
  (:: superParent (ParentMethods c))
  (:: child (Fn c String)))

(struct (GrandChildMethods gc)
  (:: superChild (ChildMethods gc))
  (:: grandchild (Fn gc Bool)))
'''

        self.assert_lowers(input_text, output_text)

    def test_lowering_class_use_for_concrete_type(self):
        input_text = '''
          (fn show__an_int ()
            (Fn String)
            (:: ((:: show_ (Fn Int String)) 12345) String))

          (class (Show_ a)
            (:: show_ (Fn a String)))

          (instance (Show_ Int)
            (fn show_ (i) "_"))
 '''

        output_text = '''
(fn make__Show_Methods__Int ()
  (Fn (Show_Methods Int))
  (:: (new Show_Methods
        (:: (\ (i) "_") (Fn Int String)))
  (Show_Methods Int)))

(fn show__an_int ()
  (Fn String)
  (:: ((:: (. (:: ((:: make__Show_Methods__Int (Fn (Show_Methods Int)))) (Show_Methods Int)) show_) (Fn Int String)) 12345) String))

(struct (Show_Methods a)
  (:: show_ (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

    def test_pass_predicates_to_class_method(self):
        input_text = '''
(class (Foldable t)
  (:: foldl (Fn (Fn b a b) b (t a) b))
  (:: elem (=> ((Eq_ a)) (Fn a (t a) Bool))))

(class (Eq_ a)
  (:: ==_ (Fn a a Bool)))

(fn pass_method_to_class (item list)
  (=> ((Foldable f) (Eq_ x)) (Fn x (f x) Bool))
  (::
    ((:: elem (Fn x (f x) Bool))
     (:: item x)
     (:: list (f x)))
    Bool))
'''

        output_text = '''
(fn pass_method_to_class (dict_Foldable_f dict_Eq__x item list)
  (Fn (FoldableMethods f) (Eq_Methods x) x (f x) Bool)
  (:: ((:: (. (:: dict_Foldable_f (FoldableMethods f)) elem)
           (Fn (Eq_Methods x) x (f x) Bool))
       (:: dict_Eq__x (Eq_Methods x))
       (:: item x)
       (:: list (f x)))
      Bool))

(struct (FoldableMethods t)
  (:: foldl (Fn (Fn b a b) b (t a) b))
  (:: elem (Fn (Eq_Methods a) a (t a) Bool)))

(struct (Eq_Methods a)
  (:: ==_ (Fn a a Bool)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

    def test_simple_instance_function(self):
        input_text = '''
          (class (Show a)
            (:: show (Fn a String)))

          (instance (Show Int)
            (fn show (i)
              "_"))
'''

        output_text = '''
          (fn make__ShowMethods__Int ()
            (Fn (ShowMethods Int))
            (:: (new ShowMethods
                  (:: (\ (i) "_") (Fn Int String)))
                (ShowMethods Int)))

          (struct (ShowMethods a)
            (:: show (Fn a String)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

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
                                        (Fn (ShowMethods Int))))
                                     (ShowMethods Int))
                                   show)
                                (Fn Int String))
                               ((::
                                   (. (::
                                        ((::
                                           make__NextMethods__Int
                                           (Fn (NextMethods Int))))
                                        (NextMethods Int))
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
        self.assert_lowers(output_text, output_text)

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
                 (:: concat (Fn String String String))
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
                      ((:: concat (Fn String String String))
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

          (struct (Pair a)
            (:: x a)
            (:: y a))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

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
        ((:: make__ParentMethods__String (Fn (ParentMethods String))))
        (ParentMethods String))
      (::
        (\ (s)
          (::
            ((:: inc (Fn Int Int))
             (::
               ((::
                  (. (:: ((:: make__ParentMethods__String (Fn (ParentMethods String))))
                         (ParentMethods String))
                     parent)
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
        self.assert_lowers(output_text, output_text)

    def test_lowering_partial_function_application(self):
        input_text = '''
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

        output_text = '''
          (fn make__Show_Methods__Int ()
            (Fn (Show_Methods Int))
            (:: (new Show_Methods
                  (:: (\ (i)
                         (:: ((:: str (Fn Int String))
                              (:: i Int))
                             String))
                      (Fn Int String)))
                (Show_Methods Int)))

          (fn use_partially_applied_function ()
            (Fn String)
            (:: ((:: ((:: get_partially_applied_function
                          (Fn (Fn Int String))))
                     (Fn Int String))
                 321)
                String))

          (fn get_partially_applied_function ()
             (Fn (Fn Int String))
             (:: (*partial*
                   (:: show__thing
                       (Fn (Show_Methods Int) Int String))
                   (:: ((:: make__Show_Methods__Int (Fn (Show_Methods Int))))
                       (Show_Methods Int)))
                 (Fn Int String)))

          (fn show__thing (dict_Show__a thing)
            (Fn (Show_Methods a) a String)
            (:: ((:: (. (:: dict_Show__a (Show_Methods a)) show_)
                     (Fn a String))
                 (:: thing a))
                String))

          (struct (Show_Methods s)
            (:: show_ (Fn s String)))
'''

        self.assert_lowers(input_text, output_text)
        self.assert_lowers(output_text, output_text)

    def assert_lowers(self, input_text, output_text):
        lowering_input = make_lowering_input(input_text)
        expected = parse_output(output_text)

        result = lowering_input.lower()

        self.assert_programs_equal(expected, result)

    def assert_programs_equal(self, expected, result):
        expected_str = show_result(expected)
        result_str = show_result(result)

        if expected_str != result_str:
            message_lines = [
                '--- Expected: ---',
                show_result(expected),
                '=================',
                '--- Result: -----',
                show_result(result),
                '=================',
            ]
        else:
            message_lines = [
                'expected matches result:',
                expected_str,
            ]
        message = '\n' + '\n'.join(message_lines)

        result_without_builtins = parse_output(result_str)
        self.assertEqual(expected, result_without_builtins, message)


def parse_output(text):
    ''' Parse a Program that's the result of lowering.

    This allows expressing the expected output as program text.

    This should be the reverse of running print_result.
    '''
    lists = parser._parse_lists(text)

    declarations = []
    dictionaries = []

    for sexpr in lists:
        if sexpr[0] == 'fn':
            fn = parser._parse_function_declaration(sexpr)
            assert fn.t.predicates == [], "did you pass input text as the output text?"
            # Remove the predicates from the function's type
            fn.t = fn.t.unqualify()
            declarations.append(fn)
        else:
            assert(sexpr[0] == 'struct')
            struct = parser._parse_struct_definition(sexpr)
            dictionaries.append(struct)

    return Program(
        from_stage='lowering',
        functions=declarations,
        structs=dictionaries,
        classes=[],
        instances=[]
    )


def print_diff(expected, result):
    print_result(expected)
    print('---')
    print_result(result)


def print_result(result):
    for lisp in result.to_lisp():
        print(syntax.render_lisp(lisp))


def show_result(result):
    assert(isinstance(result, Program))
    return '\n'.join(
        syntax.render_lisp(lisp)
        for lisp in result.to_lisp(show_builtins=False)
    )


def make_lowering_input(text):
    parse_result = parser.parse(text)
    return lowering.LoweringInput(parse_result)


if __name__ == '__main__':
    unittest.main()
