import unittest

from interpreter.parser import _parse_lists


class ParserTest(unittest.TestCase):
    def test_parses_lists(self):
        result = _parse_lists('() (:x (+ 1 2) "asdf") ;; comment\n (.)')
        expected = [[], [':x', ['+', '1', '2'], '"asdf"'], ['.']]
        self.assertEqual(expected, result)


if __name__ == '__main__':
    unittest.main()
