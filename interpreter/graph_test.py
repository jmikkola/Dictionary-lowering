import unittest

from graph import topological_order

class GraphTest(unittest.TestCase):
    def test_topological_order(self):
        self.assertEqual([], topological_order([]))
        self.assertFalse(topological_order([('a', 'b'), ('b', 'a')]))
        self.assertEqual(['a', 'b'], topological_order([('a', 'b')]))

        self.assertEqual(
            ['a', 'c', 'b', 'd', 'e'],
            topological_order([
                ('d', 'e'),
                ('c', 'd'),
                ('b', 'd'),
                ('a', 'c'),
                ('a', 'b'),
            ]),
        )

if __name__ == '__main__':
    unittest.main()
