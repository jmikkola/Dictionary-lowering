import unittest

from interpreter import inference
from interpreter import parser
from interpreter import syntax
from interpreter import types


class InferenceTest(unittest.TestCase):
    def test_split_bindings(self):
        text = '''
;; implicit function with no dependencies
(fn impl1 (x) x)

;; explicit function with no dependencies
(fn expl1 (x) (Fn Int Int) x)

;; implicit functions that form a connected component
(fn impl2 (x) (impl4 (impl3 (impl1 x))))
(fn impl3 (x) (impl2 (expl1 x)))

;; cyclic call graph that does not form a group because one function is explicitly typed
(fn impl4 (x) (impl5 x))
(fn impl5 (x) (expl2 x))
(fn expl2 (x) (Fn Int Int) (impl4 x))
'''
        program = parser.parse(text)

        explicit_typed, implicit_typed_groups = inference.split_bindings(program.functions)

        expl_names = sorted(f.name for f in explicit_typed)
        self.assertEqual(['expl1', 'expl2'], expl_names)

        impl_names = [sorted(f.name for f in group) for group in implicit_typed_groups]
        self.assertEqual([['impl1'], ['impl5'], ['impl4'], ['impl2', 'impl3']], impl_names)

    def test_get_called_functions(self):
        def call(text):
            program = parser.parse(text)
            return inference.get_called_functions(program.functions[0])

        # Basic examples
        self.assertEqual(set(), call('(fn f () 0)'))
        self.assertEqual({'f'}, call('(fn f () (f))'))
        self.assertEqual({'g'}, call('(fn f () (g))'))

        # References to arguments are not treated as references to other functions
        self.assertEqual({'a', 'b'}, call('(fn f (x y) (a b x y))'))

        # References to let-bound variables are also not included
        self.assertEqual({'a'}, call('(fn f (x) (let ((y x) (z y)) (z a x)))'))

        # Finally, variables introduced as arguments to lambda functions are not included
        self.assertEqual({'a'}, call('(fn f (x) (\ (y) (a y x)))'))

        # Other syntax forms work
        self.assertEqual({'a', 'b'}, call('(fn f (x) (if x (a) (b)))'))
        self.assertEqual({'a'}, call('(fn f (x) (new Item (a x)))'))
        self.assertEqual({'a'}, call('(fn f (x) (. (a x) field))'))
        self.assertEqual({'a'}, call('(fn f (x) (*partial* a x))'))

    def test_candidates(self):
        inf = inference.Inference(self.empty_program())

        a = types.TypeVariable('a')
        num = types.Predicate(types.TClass('Num'), types.TVariable(a))
        show = types.Predicate(types.TClass('Show'), types.TVariable(a))
        my_class = types.Predicate(types.TClass('MyClass'), types.TVariable(a))
        integral = types.Predicate(types.TClass('Integral'), types.TVariable(a))

        t_int = types.TConstructor('Int')
        t_float = types.TConstructor('Float')

        inf.add_class('MyClass', [])
        inf._add_simple_instance('MyClass', 'Int')
        inf._add_simple_instance('MyClass', 'Float')

        # Num has candidates
        candidates = inf.candidates(inference.Ambiguity(a, [num]))
        self.assertEqual([t_int, t_float], candidates)

        # Num and Show have candidates
        candidates = inf.candidates(inference.Ambiguity(a, [num, show]))
        self.assertEqual([t_int, t_float], candidates)

        # Just Show doesn't have candidates
        candidates = inf.candidates(inference.Ambiguity(a, [show]))
        self.assertEqual([], candidates)

        # Num with a user-defined type class doesn't have candidates
        candidates = inf.candidates(inference.Ambiguity(a, [num, my_class]))
        self.assertEqual([], candidates)

        # Num with Integral returns just Int
        candidates = inf.candidates(inference.Ambiguity(a, [integral, num]))
        self.assertEqual([t_int], candidates)

        # Subclasses of Num have candidates (even if they don't explicitly list Num)
        candidates = inf.candidates(inference.Ambiguity(a, [integral]))
        self.assertEqual([t_int], candidates)

        # Does not resolve an ambiguity that is not in HNF:
        t = types.TApplication(types.TConstructor('List'), [types.TVariable(a)])
        num_t = types.Predicate(types.TClass('Num'), t)
        candidates = inf.candidates(inference.Ambiguity(a, [num_t]))
        self.assertEqual([], candidates)

    def test_to_head_normal_form(self):
        inf = inference.Inference(self.empty_program())

        # A predicate already in HNF
        p = predicate('(Num a)')
        self.assertTrue(inf.in_head_normal_form(p))
        self.assertEqual([p], inf.to_head_normal_form(p))

        # Application of a type to a type variable
        p = predicate('(Num (a Int))')
        self.assertTrue(inf.in_head_normal_form(p))
        self.assertEqual([p], inf.to_head_normal_form(p))

        # A predicate with a concrete type that is given by the instances
        p = predicate('(Num Int)')
        self.assertFalse(inf.in_head_normal_form(p))
        self.assertEqual([], inf.to_head_normal_form(p))

        # A predicate that can be replaced with a simpler predicate
        p = predicate('(Show (List a))')
        self.assertFalse(inf.in_head_normal_form(p))
        simpler_predicate = predicate('(Show a)')
        self.assertEqual([simpler_predicate], inf.to_head_normal_form(p))

        # A predicate can get recursively replaced
        p = predicate('(Show (List Int))')
        self.assertFalse(inf.in_head_normal_form(p))
        self.assertEqual([], inf.to_head_normal_form(p))

        # A predicate that doesn't match any instance
        p = predicate('(Num Bool)')
        with self.assertRaises(types.TypeError):
            inf.to_head_normal_form(p)

    def test_find_ambiguities(self):
        inf = inference.Inference(self.empty_program())

        a = types.TypeVariable('a')
        b = types.TypeVariable('b')
        c = types.TypeVariable('c')

        # A predicate isn't ambiguous if the type variable is part of the binding's type
        type_variables = [a]
        predicates = [predicate('(Num a)')]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        self.assertEqual([], ambiguities)

        # A predicate is ambiguous if the type variable is not part of the binding's type
        type_variables = [a]
        predicates = [predicate('(Num b)')]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        self.assertEqual([inference.Ambiguity(b, predicates)], ambiguities)

        # Ambiguities are grouped by type variable
        type_variables = [a]
        predicates = [
            predicate('(Num a)'),
            predicate('(Num b)'),
            predicate('(Ord b)'),
            predicate('(Show c)'),
        ]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        expected = [
            inference.Ambiguity(b, [predicates[1], predicates[2]]),
            inference.Ambiguity(c, [predicates[3]]),
        ]
        self.assertEqual(expected, ambiguities)

    def test_by_instances(self):
        inf = inference.Inference(self.empty_program())

        # Returns None when there's no matching instance
        p = predicate('(Num Bool)')
        self.assertIsNone(inf.by_instances(p))

        # Returns an empty list of predicates when there's a matching instance with
        # no additional predicates.
        p = predicate('(Num Int)')
        self.assertEqual([], inf.by_instances(p))

        # Return a predicate when the instance demands additional predicates
        p = predicate('(Show (List a))')
        expected = predicate('(Show a)')
        self.assertEqual([expected], inf.by_instances(p))

    def test_quanification(self):
        a = types.TypeVariable('a')
        b = types.TypeVariable('b')

        # Generalizes over the type variables in a function's type
        qt = qualified('(Fn (List a) b)')
        scheme = types.Scheme.quantify([a, b], qt)

        func_type = types.make_function_type(
            [types.TApplication(types.TConstructor('List'), [types.TGeneric(0)])],
            types.TGeneric(1)
        )
        qt2 = types.Qualified([], func_type)
        expected = types.Scheme(n_vars=2, qualified=qt2)
        self.assertEqual(expected, scheme)

        # Does not generalize over a variable if it is not free in the type
        qt = qualified('(Fn (List a) b)')
        scheme = types.Scheme.quantify([b], qt)

        func_type = types.make_function_type(
            [types.TApplication(types.TConstructor('List'), [types.TVariable(a)])],
            types.TGeneric(0)
        )
        qt2 = types.Qualified([], func_type)
        expected = types.Scheme(n_vars=1, qualified=qt2)
        self.assertEqual(expected, scheme)

        # Generalizes predicates in the function's type
        qt = qualified('(=> ((Show a)) (Fn (List a) String))')
        scheme = types.Scheme.quantify([a], qt)

        func_type = types.make_function_type(
            [types.TApplication(types.TConstructor('List'), [types.TGeneric(0)])],
            types.TConstructor('String')
        )
        predicate = types.Predicate(types.TClass('Show'), types.TGeneric(0))
        qt2 = types.Qualified([predicate], func_type)
        expected = types.Scheme(n_vars=1, qualified=qt2)
        self.assertEqual(expected, scheme)

    def test_instantiate(self):
        inf = inference.Inference(self.empty_program())

        a = types.TypeVariable('a')
        b = types.TypeVariable('b')
        qt = qualified('(Fn (Fn a b) (List a) (List b))')
        scheme = types.Scheme.quantify([a, b], qt)

        result = inf.instantiate(scheme)

        qt = qualified('(Fn (Fn t1 t2) (List t1) (List t2))')
        self.assertEqual(qt, result)

        # Instantiating again gives fresh type variables
        result = inf.instantiate(scheme)

        qt = qualified('(Fn (Fn t3 t4) (List t3) (List t4))')
        self.assertEqual(qt, result)

    def test_entails(self):
        inf = inference.Inference(self.empty_program())

        # Predicates that are given by instances are entailed by themselves
        p = predicate('(Num Int)')
        self.assertTrue(inf.entails([], p))

        p = predicate('(Show (List Int))')
        self.assertTrue(inf.entails([], p))

        # (Num Bool) is not entailed because there's no instance for that
        p = predicate('(Num Bool)')
        self.assertFalse(inf.entails([], p))

        # (Show (List a)) is not entailed because the predicate (Show a) isn't given
        p = predicate('(Show (List a))')
        self.assertFalse(inf.entails([], p))

        # A predicate is entailed if it appears in the list
        p = predicate('(Num a)')
        self.assertTrue(inf.entails([p], p))

        # Or if there's an instance whose predicates are in the list
        p = predicate('(Show (List a))')
        self.assertTrue(inf.entails([predicate('(Show a)')], p))

        # Or if a predicate in the list has a superclass that matches
        p = predicate('(Eq a)')
        self.assertTrue(inf.entails([predicate('(Ord a)')], p))

        # A predicate that doesn't match:
        p = predicate('(Eq a)')
        self.assertFalse(inf.entails([predicate('(Ord b)')], p))

    def test_simplify(self):
        inf = inference.Inference(self.empty_program())

        self.assertEqual([], inf.simplify([]))

        # Keeps predicates that are not entailed
        p = predicate('(Num a)')
        self.assertEqual([p], inf.simplify([p]))

        # Removes predicates that are given
        p = predicate('(Num Int)')
        self.assertEqual([], inf.simplify([p]))

        # Deduplicates predicates
        p = predicate('(Num a)')
        self.assertEqual([p], inf.simplify([p, p]))

    def test_reduce(self):
        inf = inference.Inference(self.empty_program())

        # Simplifies predicates
        show_list = predicate('(Show (List a))')
        show_a = predicate('(Show a)')
        self.assertEqual([show_a], inf.reduce([show_list]))

        # Keeps just the child class if a parent and child are both listed
        eq_a = predicate('(Eq a)')
        ord_a = predicate('(Ord a)')
        self.assertEqual([ord_a], inf.reduce([eq_a, ord_a]))

    def test_infer_literal(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions()

        predicates, t = inf.infer_expression(assumptions, expression('"abc"'), [])
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('String'), t)

        predicates, t = inf.infer_expression(assumptions, expression('123.45'), [])
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('Float'), t)

        predicates, t = inf.infer_expression(assumptions, expression('123'), [])
        self.assertEqual([predicate('(Num t1)')], predicates)
        self.assertEqual(types.TVariable.from_varname('t1'), t)

        # Infering that again gives new type variables
        predicates, t = inf.infer_expression(assumptions, expression('123'), [])
        self.assertEqual([predicate('(Num t2)')], predicates)
        self.assertEqual(types.TVariable.from_varname('t2'), t)

    def test_infer_variable(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({
            'x': types.Scheme.quantify([], qualified('Int')),
            'y': types.Scheme.quantify(
                [types.TypeVariable('a')],
                qualified('(=> ((Eq a)) (Fn a a))')
            ),
        })

        predicates, t = inf.infer_expression(assumptions, expression('x'), [])
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('Int'), t)

        predicates, t = inf.infer_expression(assumptions, expression('y'), [])
        self.assertEqual([predicate('(Eq t1)')], predicates)
        self.assertEqual(type_('(Fn t1 t1)'), t)

        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expression('z'), [])

    def test_infer_call(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({
            'f': types.Scheme.quantify(
                [types.TypeVariable('a')],
                qualified('(=> ((Eq a)) (Fn a a))')
            ),
        })

        expr = expression('(f "abc")')

        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([predicate('(Eq t1)')], predicates)
        predicates_subbed = inf.substitution.apply_to_list(predicates)
        self.assertEqual([predicate('(Eq String)')], predicates_subbed)

        self.assertEqual(type_('t2'), t)
        t_subbed = t.apply(inf.substitution)
        self.assertEqual(type_('String'), t_subbed)

        self.assertEqual(expr.get_type(), t)

    def test_infer_construct(self):
        text = '''
(struct Point (:: x Int) (:: y Int))
'''
        program = parser.parse(text)
        inf = inference.Inference(program)

        assumptions = inference.Assumptions()

        expr = expression("(new Point 1 2)")
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([predicate('(Num t1)'), predicate('(Num t2)')], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Point'), t)

        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expression("(new Point 1 \"a\")"), [])

        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expression("(new Point 1 2 3)"), [])

    def test_infer_generic_struct(self):
        text = '''
(struct (Pair a b) (:: first a) (:: second b))
'''
        program = parser.parse(text)
        inf = inference.Inference(program)

        assumptions = inference.Assumptions()

        expr = expression('(new Pair "abc" false)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('(Pair String Bool)'), t)

        expr = expression('(new Pair (new Pair "abc" 123.0) false)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('(Pair (Pair String Float) Bool)'), t)

    def test_infer_access(self):
        text = '''
(struct Point (:: x Int) (:: y Int))
(struct (Pair a b) (:: first a) (:: second b))
'''
        program = parser.parse(text)
        inf = inference.Inference(program)
        assumptions = inference.Assumptions()

        expr = expression('(. (new Point 1 2) x)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        predicates = inf.substitution.apply_to_list(predicates)
        self.assertEqual([predicate('(Num Int)'), predicate('(Num Int)')], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Int'), t)

        # Nested access works
        expr = expression('(. (. (new Pair (new Pair "abc" 123.0) false) first) second)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Float'), t)

        with self.assertRaises(types.TypeError):
            # z is not a field of Point
            expr = expression('(. (new Point 1 2) z)')
            inf.infer_expression(assumptions, expr, [])

        with self.assertRaises(types.TypeError):
            expr = expression('(. "foo" x)')
            inf.infer_expression(assumptions, expr, [])

    def test_infer_let(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({
            '*': types.Scheme.quantify(
                [types.TypeVariable('a')],
                qualified('(=> ((Num a)) (Fn a a a))')
            ),
            'var': types.Scheme.quantify(
                [],
                qualified('a')
            ),
            'show': types.Scheme.quantify(
                [types.TypeVariable('a')],
                qualified('(=> ((Show a)) (Fn a String))')
            ),
        })

        # A simple let expression
        expr = expression('(let ((x "abc") (y 123.0)) x)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('String'), t)

        # A let expression where one value depends on the other
        expr = expression('(let ((x 123.0) (y (* 2.0 x))) y)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        predicates = inf.substitution.apply_to_list(predicates)
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Float'), t)

        # Can surface predicates for variables referenced in the bindings
        expr = expression('(let ((x (show var))) x)')
        inf = inference.Inference(self.empty_program())
        predicates, t = inf.infer_expression(assumptions, expr, [])
        predicates = inf.substitution.apply_to_list(predicates)
        self.assertEqual([predicate('(Show t2)')], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('String'), t)

        # A variable can be instantiated at multiple different types
        expr = expression('(let ((id (\ (x) x))) ((id id) false))')
        inf = inference.Inference(self.empty_program())
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Bool'), t)

        # Other cases that would be interesting to test:
        # - Mutually recursive functions defined in let bindings
        # - Let bindings that retain their predicates
        #   (e.g. `(x (\y (show y)))`)
        # - A type that can't be instantiated in different ways due to defaulting

    def test_infer_if(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({
            'var': types.Scheme.quantify(
                [],
                qualified('a')
            ),
        })

        # If expression with matching types
        expr = expression('(if true 1.0 2.0)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Float'), t)

        # The test is forced to have the type Bool
        expr = expression('(if var var var)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('Bool'), t)

        # Fails if the branches have different types
        expr = expression('(if true "x" 2.0)')
        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expr, [])

        # Fails if the test isn't a boolean
        expr = expression('(if "x" 2 3)')
        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expr, [])

    def test_infer_lambda(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({})

        # Very simple, no argument lambda
        expr = expression('(\ () true)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        self.assertEqual(type_('(Fn Bool)'), t)

        # A lambda that takes an argument
        expr = expression('(\ (x) x)')
        predicates, t = inf.infer_expression(assumptions, expr, [])
        self.assertEqual([], predicates)
        self.assertEqual(type_('(Fn t1 t1)'), t)

    def test_get_assumptions_for_functions(self):
        text = '''
(class (Next n)
  (:: next (Fn n n)))
'''
        inf = inference.Inference(parser.parse(text))
        assumptions = inf.get_assumptions_for_functions([])

        gen0 = types.TGeneric(0)
        p = types.Predicate(types.TClass('Next'), gen0)
        t = types.make_function_type([gen0], gen0)
        expected = {
            'next': types.Scheme(1, types.Qualified([p], t))
        }
        self.assertEqual(expected, assumptions)

        # Make use of those assumptions:
        expr = expression('(\ (x) (next x))')
        predicates, t = inf.infer_expression(inference.Assumptions(assumptions), expr, [])
        predicates = inf.substitution.apply_to_list(predicates)
        self.assertEqual([predicate('(Next t2)')], predicates)
        t = t.apply(inf.substitution)
        self.assertEqual(type_('(Fn t2 t2)'), t)

    def test_infer_single_implicit_function(self):
        text = '''
(fn identity (x) x)
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.functions[0]
        qt = f.t.apply(inf.substitution)

        expected = qualified('(Fn t2 t2)')
        self.assertEqual(expected, qt)

    def test_infer_implicit_function_with_predicates(self):
        text = '''
(fn fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.functions[0]
        qt = f.t.apply(inf.substitution)

        expected = qualified('(=> ((Num t6) (Ord t6)) (Fn t6 t6))')
        self.assertEqual(expected, qt)

    def test_resolvable_ambiguity(self):
        text = '''
(fn f () (show 1))
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.functions[0]
        scheme = f.t.apply(inf.substitution)

        expected = qualified('(Fn String)')
        self.assertEqual(expected, scheme)

    def test_unresolvable_ambiguity(self):
        text = '''
(fn f () (show (read "")))
'''
        inf = inference.Inference(parser.parse(text))

        with self.assertRaises(types.TypeError) as cm:
            inf.infer()
        self.assertIn('No default type', str(cm.exception))

    def test_explicit_typed_function(self):
        text = '''
(fn f (x) (Fn Int Int) x)
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.functions[0]
        scheme = f.t.apply(inf.substitution)

        expected = qualified('(Fn Int Int)')
        self.assertEqual(expected, scheme)

    def test_explicit_typed_function_with_predicates(self):
        text = '''
(fn f (x y)
  (=> ((Ord a)) (Fn a a Bool))
  (or (< x y) (> x y)))
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.functions[0]
        qt = f.t.apply(inf.substitution)

        expected = qualified('(=> ((Ord t)) (Fn t t Bool))')
        self.assert_qualifieds_equal(expected, qt)

    def test_explicit_typed_function_too_general_type_sig(self):
        text = '''
(fn f (x) (Fn a a)
    "a string not any type")
'''
        inf = inference.Inference(parser.parse(text))

        with self.assertRaises(types.TypeError) as cm:
            inf.infer()
        self.assertIn('Type signature too general', str(cm.exception))

    def test_explicit_typed_function_with_insufficient_predicates(self):
        text = '''
(fn f (x y)
  (=> ((Eq a)) (Fn a a Bool))
  (or (< x y) (> x y)))
'''
        program = parser.parse(text)

        with self.assertRaises(types.TypeError) as cm:
            inference.infer_types(program)
        self.assertIn('is missing predicates', str(cm.exception))

    def test_checks_types_of_instance_methods(self):
        text = '''
(struct Point
  (:: x Int)
  (:: y Int))

(class (ToString a)
  (:: to_string (Fn a String)))

(instance (ToString Point)
  (fn to_string (p) "some string"))
'''
        inf = inference.Inference(parser.parse(text))

        program = inf.infer()
        f = program.instances[0].method_impls[0]
        qt = f.t.apply(inf.substitution)

        expected = qualified('(Fn Point String)')
        self.assertEqual(expected, qt)

    def test_checks_types_of_instance_methods_with_incorrect_type(self):
        text = '''
(struct Point
  (:: x Int)
  (:: y Int))

(class (ToString a)
  (:: to_string (Fn a String)))

(instance (ToString Point)
  (fn to_string (p) false))
'''
        inf = inference.Inference(parser.parse(text))

        with self.assertRaises(types.TypeError):
            inf.infer()

    def test_checks_type_of_instance_with_predicates(self):
        text = '''
(struct (Pair a b)
  (:: a a)
  (:: b b))

(class (Show_ a)
  (:: show_ (Fn a String)))

(instance (=> ((Show_ ta) (Show_ tb)) (Show_ (Pair ta tb)))
  (fn show_ (pair)
    (concat (show_ (. pair a)) (concat ", " (show_ (. pair b))))))
'''
        inf = inference.Inference(parser.parse(text))
        inf.infer()

    def test_show_pair(self):
        text = '''
(struct (Pair a b)
  (:: a a)
  (:: b b))

(class (Show_ a)
  (:: show_ (Fn a String)))

(fn my_show (pair)
  (=> ((Show_ ta) (Show_ tb)) (Fn (Pair ta tb) String))
  (concat (show_ (. pair a)) (concat ", " (show_ (. pair b)))))
'''
        inf = inference.Inference(parser.parse(text))
        program = inf.infer()

        self.assert_qualifieds_equal(
            qualified('(=> ((Show_ ta) (Show_ tb)) (Fn (Pair ta tb) String))'),
            program.get_function('my_show').t
        )

    def test_pair_access(self):
        text = '''
(struct (Pair a b)
  (:: a a)
  (:: b b))

(fn get_a (pair)
  (Fn (Pair ta tb) ta)
  (. pair a))
'''
        inf = inference.Inference(parser.parse(text))
        program = inf.infer()

        self.assert_qualifieds_equal(
            qualified('(Fn (Pair x y) x)'),
            program.get_function('get_a').t
        )

    def test_sets_types_of_expressions_in_explititly_typed_fn(self):
        text = '''
(fn fib (n)
  (Fn Int Int)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
'''
        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_text = '''
(fn fib (n)
  (=> () (Fn Int Int))
  (:: (if (:: ((:: < (Fn Int Int Bool))
               (:: n Int)
               (:: 2 Int))
              Bool)
          (:: n Int)
          (:: ((:: + (Fn Int Int Int))
               (:: ((:: fib (Fn Int Int))
                    (:: ((:: - (Fn Int Int Int))
                         (:: n Int)
                         (:: 1 Int))
                        Int))
                   Int)
               (:: ((:: fib (Fn Int Int))
                    (:: ((:: - (Fn Int Int Int))
                         (:: n Int)
                         (:: 2 Int))
                        Int))
                   Int))
              Int))
      Int))
'''
        expected_lisp = parser._parse_one_list(expected_text)

        self.assertEqual(syntax.render_lisp(expected_lisp), syntax.render_lisp(function.to_lisp()))

    def test_sets_types_of_expressions_in_implicitly_typed_fn(self):
        text = '''
(fn identity (x) x)
'''
        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_text = '''
(fn identity (x)
  (=> () (Fn t2 t2))
  (:: x t2))
'''
        expected_lisp = parser._parse_one_list(expected_text)

        self.assertEqual(expected_lisp, function.to_lisp())

    def test_applies_defaulting(self):
        text = '''
(fn main ()
  (print (+ 1 2)))
'''
        # 1 and 2 are ambiguously typed (it could be any Num type)
        # so defaulting should pick a concrete type for it
        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_text = '''
(fn main ()
  (=> () (Fn Void))
  (:: ((:: print (Fn Int Void))
       (:: ((:: + (Fn Int Int Int))
            (:: 1 Int)
            (:: 2 Int))
           Int))
      Void))
'''
        expected_lisp = parser._parse_one_list(expected_text)
        self.assertEqual(expected_lisp, function.to_lisp())

    def test_defaulting_predicates_from_user_defined_function(self):
        text = '''
(fn main ()
  (Fn Void)
  (print (add1 2)))

(fn add1 (n)
  (+ 1 n))
'''
        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]
        # from interpreter import syntax
        # print(syntax.render_lisp(function.to_lisp()))

        expected_text = '''
(fn main ()
  (=> () (Fn Void))
  (:: ((:: print (Fn Int Void))
       (:: ((:: add1 (Fn Int Int))
            (:: 2 Int))
           Int))
      Void))
'''
        expected_lisp = parser._parse_one_list(expected_text)

        self.assertEqual(expected_lisp, function.to_lisp())

    def test_instance_with_concrete_type(self):
        text = '''
(class (Parent p)
  (:: parent (Fn p Int)))

(instance (Parent String)
  (fn parent (s)
    (length s)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        instance = program.instances[0]
        function = instance.method_impls[0]

        expected_type = qualified('(Fn String Int)')
        self.assertEqual(expected_type, function.t)

    def test_child_and_parent_class(self):
        text = '''
(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c)
  superclasses (Parent)
  (:: child (Fn c Int)))

(instance (Parent String)
  (fn parent (s)
    (length s)))

(instance (Child String)
  (fn child (s)
    (inc (parent s))))

(fn use-child-class (x)
  (child x))

(fn main ()
  (print (use-child-class "xyz")))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        inst = next(i for i in program.instances if i.get_class().name == 'Child')
        method = inst.method_impls[0]

        expected_type = qualified('(Fn String Int)')
        self.assertEqual(expected_type, method.t)

    def test_user_defined_types(self):
        text = '''
(fn f (x y)
  (== x (:: y Int)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_type = qualified('(Fn Int Int Bool)')
        self.assertEqual(expected_type, function.t)

    def test_allows_matching_user_defined_type(self):
        text = '''
(fn f ()
  (:: (\ (x) x) (Fn a a)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_type = qualified('(Fn (Fn a a))')
        self.assertEqual(expected_type, function.t)

    def test_allows_narrower_user_defined_type(self):
        text = '''
(fn f ()
  (:: (\ (x) x) (Fn String String)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        expected_type = qualified('(Fn (Fn String String))')
        self.assertEqual(expected_type, function.t)

    def test_rejects_wider_user_defined_type(self):
        text = '''
(fn f ()
  (:: (\ (x) x) (Fn a b)))
'''
        program = parser.parse(text)

        with self.assertRaises(types.TypeError):
            inference.infer_types(program)

    def test_allows_main_with_valid_type(self):
        text = '''
(fn main ()
   (print "hi"))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)

        function = program.functions[0]

        # might as well check the type while we're here
        expected_type = qualified('(Fn Void)')
        self.assertEqual(expected_type, function.t)

    def test_rejects_main_with_invalid_type(self):
        text = '''
(fn main (x y z)
   (print "hi"))
'''

        program = parser.parse(text)
        with self.assertRaises(types.TypeError):
            inference.infer_types(program)

    def test_allows_main_that_returns_a_value(self):
        text = '''
(fn main ()
   "hi")
'''

        program = parser.parse(text)
        inference.infer_types(program)

    def test_infers_mutually_recursive_let_bindings(self):
        text = '''
(fn collatz-seq (n)
  (let ((add-num (\ (n rest)
                    (concat (str n) (concat "," rest))))
        (odd  (\ (n) (ctz (+ (* n 3) 1))))
        (even (\ (n) (ctz (/ n 2))))
        (ctz  (\ (n)
                (if (== n 1)
                  "1"
                  (add-num
                    n
                    (if (== (% n 2) 0) (even n) (odd n)))))))
      (ctz n)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)
        function = program.functions[0]
        expected = qualified('(=> ((Integral t41)) (Fn t41 String))')
        self.assertEqual(expected, function.t)

    def test_let_binding_with_predicates(self):
        text = '''
(fn use-square (x)
  (let ((square (\ (n) (* n n))))
    (square x)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)
        function = program.functions[0]
        expected = qualified('(=> ((Num t7)) (Fn t7 t7))')
        self.assertEqual(expected, function.t)

    def test_let_binding_with_multiple_instantiations(self):
        text = '''
(fn use-id (x)
  (let ((id (\ (n) n)))
    ((id id) x)))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)
        function = program.functions[0]
        expected = qualified('(Fn t6 t6)')
        self.assertEqual(expected, function.t)

    def test_let_binding_with_predicates_and_multiple_instantiations(self):
        text = '''
(fn use-to-str ()
  (let ((to-str (\ (n) (show n))))
    (concat (to-str true) (to-str ""))))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)
        function = program.functions[0]
        expected = qualified('(Fn String)')
        self.assertEqual(expected, function.t)

    def test_let_binding_with_deferred_predicates(self):
        text = '''
(fn foo (x)
  (let ((is-three (== 3 x)))
     is-three))
'''

        program = parser.parse(text)
        program = inference.infer_types(program)
        function = program.functions[0]
        expected = qualified('(=> ((Num t4)) (Fn t4 Bool))')
        self.assertEqual(expected, function.t)

    def test_infers_mutually_recursive_functions(self):
        text = '''
(fn add-num (n rest)
  (concat (show n) (concat ", " rest)))

(fn odd (n)
  (collatz (+ (* 3 n) 1)))

(fn even (n)
  (collatz (/ n 2)))

(fn collatz (n)
  (if (== n 1)
    "1"
    (let ((rest (if (== (% n 2) 0)
                    (even n)
                    (odd n))))
      (add-num n rest))))
'''
        program = parser.parse(text)
        program = inference.infer_types(program)

        self.assert_qualifieds_equal(
            qualified('(=> ((Show a)) (Fn a String String))'),
            program.get_function('add-num').t
        )
        self.assert_qualifieds_equal(
            qualified('(=> ((Integral a)) (Fn a String))'),
            program.get_function('odd').t
        )
        self.assert_qualifieds_equal(
            qualified('(=> ((Integral a)) (Fn a String))'),
            program.get_function('even').t
        )
        self.assert_qualifieds_equal(
            qualified('(=> ((Integral a)) (Fn a String))'),
            program.get_function('collatz').t
        )

    def test_function_with_simplifyable_predicates(self):
        text = '''
(fn f (a b)
  (if (== a b) (show a) (show (* 2 (% a b)))))
'''
        program = parser.parse(text)
        program = inference.infer_types(program)

        self.assert_qualifieds_equal(
            qualified('(=> ((Integral a)) (Fn a a String))'),
            program.get_function('f').t
        )

    def empty_program(self):
        return parser.parse('')

    def assert_qualifieds_equal(self, expected, actual):
        # Make sure the expected type can be matched to the actual
        sub = types.match(expected.t, actual.t)
        # also make sure the actual can be matched to the expected type
        # to ensure that the expected type isn't more general
        types.match(actual.t, expected.t)

        # Check that they are equal now to ensure that the predicates match
        self.assertEqual(expected.apply(sub), actual)

def predicate(text):
    return parser._parse_predicate(parser._parse_one_list(text))


def qualified(text):
    return parser._parse_qualified_type(parser._parse_one_list(text))


def expression(text):
    return parser._parse_expression(parser._parse_one_list(text))


def type_(text):
    return parser._parse_type(parser._parse_one_list(text))
