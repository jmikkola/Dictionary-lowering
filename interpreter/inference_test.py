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

        pred = lambda cls, t: types.Predicate(types.TClass(cls), t)

        # A predicate already in HNF
        predicate = pred('Num', types.TVariable.from_varname('a'))
        self.assertTrue(inf.in_head_normal_form(predicate))
        self.assertEqual([predicate], inf.to_head_normal_form(predicate))

        # Application of a type to a type variable
        t = types.TApplication(
            types.TVariable.from_varname('a'),
            [types.TConstructor('Int')]
        )
        predicate = pred('Num', t)
        self.assertTrue(inf.in_head_normal_form(predicate))
        self.assertEqual([predicate], inf.to_head_normal_form(predicate))

        # A predicate with a concrete type that is given by the instances
        predicate = pred('Num', types.TConstructor('Int'))
        self.assertFalse(inf.in_head_normal_form(predicate))
        self.assertEqual([], inf.to_head_normal_form(predicate))

        # A predicate that can be replaced with a simpler predicate
        t = types.TApplication(
            types.TConstructor('List'),
            [types.TVariable.from_varname('a')]
        )
        predicate = pred('Show', t)
        self.assertFalse(inf.in_head_normal_form(predicate))
        simpler_predicate = pred('Show', types.TVariable.from_varname('a'))
        self.assertEqual([simpler_predicate], inf.to_head_normal_form(predicate))

    def test_find_ambiguities(self):
        inf = inference.Inference(self.empty_program())

        a = types.TypeVariable('a')
        b = types.TypeVariable('b')
        c = types.TypeVariable('c')

        # A predicate isn't ambiguous if the type variable is part of the binding's type
        type_variables = [a]
        predicates = [types.Predicate(types.TClass('Num'), types.TVariable(a))]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        self.assertEqual([], ambiguities)

        # A predicate is ambiguous if the type variable is not part of the binding's type
        type_variables = [a]
        predicates = [types.Predicate(types.TClass('Num'), types.TVariable(b))]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        self.assertEqual([inference.Ambiguity(b, predicates)], ambiguities)

        # Ambiguities are grouped by type variable
        type_variables = [a]
        predicates = [
            types.Predicate(types.TClass('Num'), types.TVariable(a)),
            types.Predicate(types.TClass('Num'), types.TVariable(b)),
            types.Predicate(types.TClass('Ord'), types.TVariable(b)),
            types.Predicate(types.TClass('Show'), types.TVariable(c)),
        ]
        ambiguities = inf.find_ambiguities(type_variables, predicates)
        expected = [
            inference.Ambiguity(b, [predicates[1], predicates[2]]),
            inference.Ambiguity(c, [predicates[3]]),
        ]
        self.assertEqual(expected, ambiguities)


    def test_by_instances(self):
        pass

    def test_instantiate(self):
        pass

    def test_generalize(self):
        pass

    def test_entails(self):
        pass

    def empty_program(self):
        return parser.parse('')
