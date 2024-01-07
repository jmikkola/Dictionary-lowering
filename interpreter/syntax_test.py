import unittest

from interpreter.syntax import (
    render_lisp,
)


class SyntaxTest(unittest.TestCase):
    def test_renders_lisp(self):
        lisp = ['f', ['x', 'y'], [], '\"x\"']
        result = render_lisp(lisp)
        expected = '(f (x y) () "x")'
        self.assertEqual(expected, result)


if __name__ == '__main__':
    unittest.main()

