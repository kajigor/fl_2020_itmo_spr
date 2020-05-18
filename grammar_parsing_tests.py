import unittest
from src.grammar_parsing import grammar_parser
from src.grammar import Grammar


BRACKET_GRAMMAR_PATH = "grammars/bracket_grammar.txt"
BRACKET_GRAMMAR = \
"""START -> o START c START | EPSILON"""
FROM_EPSILON_PATH = "grammars/from_epsilon_rule.txt"
LARGE_GRAMMAR_PATH = "grammars/large_grammar.txt"
LARGE_GRAMMAR = \
"""START -> A | B | A B | C e
A -> B | a A | EPSILON
B -> C A c A | C
C -> b b"""

class TestGrammarParsing(unittest.TestCase):

    def test_bracket_grammar(self):
        with open(BRACKET_GRAMMAR_PATH) as istream:
            grammar = Grammar(grammar_parser.parse(istream.read()))
        self.assertEqual(BRACKET_GRAMMAR,
                         grammar.__repr__())

    def test_from_epsilon_fail(self):
        with open(FROM_EPSILON_PATH) as istream:
            grammar_as_list = grammar_parser.parse(istream.read())
        self.assertIsNone(grammar_as_list)

    def test_large_grammar(self):
        with open(LARGE_GRAMMAR_PATH) as istream:
            grammar = Grammar(grammar_parser.parse(istream.read()))
        self.assertEqual(LARGE_GRAMMAR,
                         grammar.__repr__())

if __name__ == "__main__":
    unittest.main()
