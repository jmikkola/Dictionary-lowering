import unittest

from interpreter import graph


class GraphTest(unittest.TestCase):
    def test_topological_order(self):
        self.assertEqual([], graph.topological_order([]))
        self.assertFalse(graph.topological_order([('a', 'b'), ('b', 'a')]))
        self.assertEqual(['a', 'b'], graph.topological_order([('a', 'b')]))

        self.assertEqual(
            ['a', 'c', 'b', 'd', 'e'],
            graph.topological_order([
                ('d', 'e'),
                ('c', 'd'),
                ('b', 'd'),
                ('a', 'c'),
                ('a', 'b'),
            ]),
        )

    def test_strongly_connected_components(self):
        # Use the example from https://en.wikipedia.org/wiki/Strongly_connected_component
        g = {
            'a': ['b'],
            'b': ['c', 'e', 'f'],
            'c': ['d', 'g'],
            'd': ['c', 'h'],
            'e': ['a', 'f'],
            'f': ['g'],
            'g': ['f'],
            'h': ['d', 'g'],
        }

        components = graph.strongly_connected_components(g)
        # Sort components to make this easy to test
        components = [sorted(c) for c in components]

        expected = [
            ['f', 'g'],
            ['c', 'd', 'h'],
            ['a', 'b', 'e'],
        ]
        self.assertEqual(expected, components)


if __name__ == '__main__':
    unittest.main()
