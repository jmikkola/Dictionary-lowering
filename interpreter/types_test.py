import unittest


from interpreter import types
from interpreter import parser


class TypesTest(unittest.TestCase):
    def test_match_matching_types(self):
        l = parse_type('(Pair a b)')
        r = parse_type('(Pair Int String)')
        result = types.match(l, r)

        expected = {
            types.TypeVariable('a'): parse_type('Int'),
            types.TypeVariable('b'): parse_type('String'),
        }
        self.assertEqual(expected, result.substitutions)

    def test_match_non_matching_types(self):
        cases = [
            ('(Pair Int String)', 'Pair'),
            ('(Pair Int String)', '(Pair a b)'),
            ('(Pair a a)', '(Pair Int String)'),
            ('(Pair a Int)', '(Pair String b)'),
        ]

        for (l_text, r_text) in cases:
            l = parse_type(l_text)
            r = parse_type(r_text)
            with self.assertRaises(types.TypeError):
                types.match(l, r)

    def test_unifies_types(self):
        l = parse_type('(Fn a a Bool)')
        r = parse_type('(Fn c b b)')
        result = types.most_general_unifier(l, r)

        expected = {
            types.TypeVariable('a'): parse_type('Bool'),
            types.TypeVariable('b'): parse_type('Bool'),
            types.TypeVariable('c'): parse_type('Bool'),
        }
        self.assertEqual(expected, result.substitutions)

    def test_occurs_check(self):
        l = parse_type('(Fn b b)')
        r = parse_type('(Fn a (Fn a a))')

        with self.assertRaises(types.TypeError, msg='Occurs check fails'):
            types.most_general_unifier(l, r)

    def test_cannot_unify(self):
        cases = [
            ('String', 'Int'),
            ('(Pair Int String)', 'Pair'),
            ('(Pair a a)', '(Pair Int String)'),
            ('(Pair Int a)', '(Pair a String)'),
            ('(Fn Int Int)', '(Fn Int Int Int)'),
        ]

        for (l_text, r_text) in cases:
            l = parse_type(l_text)
            r = parse_type(r_text)
            with self.assertRaises(types.TypeError):
                types.most_general_unifier(l, r)


def parse_type(text):
    return parser._parse_type(parser._parse_lists(text)[0])
