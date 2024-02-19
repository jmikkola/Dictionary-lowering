import unittest

from interpreter import inference
from interpreter import parser


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