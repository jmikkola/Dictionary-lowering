import unittest

from interpreter import inference
from interpreter import parser
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

        predicates, t = inf.infer_expression(assumptions, expression('"abc"'))
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('String'), t)

        predicates, t = inf.infer_expression(assumptions, expression('123.45'))
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('Float'), t)

        predicates, t = inf.infer_expression(assumptions, expression('123'))
        self.assertEqual([predicate('(Num t1)')], predicates)
        self.assertEqual(types.TVariable.from_varname('t1'), t)

        # Infering that again gives new type variables
        predicates, t = inf.infer_expression(assumptions, expression('123'))
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

        predicates, t = inf.infer_expression(assumptions, expression('x'))
        self.assertEqual([], predicates)
        self.assertEqual(types.TConstructor('Int'), t)

        predicates, t = inf.infer_expression(assumptions, expression('y'))
        self.assertEqual([predicate('(Eq t1)')], predicates)
        self.assertEqual(type_('(Fn t1 t1)'), t)

        with self.assertRaises(types.TypeError):
            inf.infer_expression(assumptions, expression('z'))

    def test_infer_call(self):
        inf = inference.Inference(self.empty_program())
        assumptions = inference.Assumptions({
            'f': types.Scheme.quantify(
                [types.TypeVariable('a')],
                qualified('(=> ((Eq a)) (Fn a a))')
            ),
        })

        expr = expression('(f "abc")')

        predicates, t = inf.infer_expression(assumptions, expr)
        self.assertEqual([predicate('(Eq t1)')], predicates)
        predicates_subbed = inf.substitution.apply_to_list(predicates)
        self.assertEqual([predicate('(Eq String)')], predicates_subbed)

        self.assertEqual(type_('t2'), t)
        t_subbed = t.apply(inf.substitution)
        self.assertEqual(type_('String'), t_subbed)

        self.assertEqual(expr.get_type(), t)

    def empty_program(self):
        return parser.parse('')


def predicate(text):
    return parser._parse_predicate(parser._parse_one_list(text))


def qualified(text):
    return parser._parse_qualified_type(parser._parse_one_list(text))


def expression(text):
    return parser._parse_expression(parser._parse_one_list(text))


def type_(text):
    return parser._parse_type(parser._parse_one_list(text))