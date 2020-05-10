package grammar

import grammar.model.CFGrammar

interface GrammarParser {
    fun parse(filename: String): CFGrammar
}
