package grammar

import antlr.CFGrammarLexer
import antlr.CFGrammarParser
import grammar.model.CFGrammar
import org.antlr.v4.runtime.*

class GrammarParserImpl : GrammarParser {
    private val parser = CFGrammarParser(null)

    override fun parse(filename: String): CFGrammar {
        val stream = CharStreams.fromFileName(filename)
        return parse(stream)
    }

    private fun parse(stream: CharStream): CFGrammar {
        val lexer = CFGrammarLexer(stream)
        val tokens = CommonTokenStream(lexer)
        parser.inputStream = tokens
        val root = parser.cfgrammar()
        check(parser.numberOfSyntaxErrors == 0) { "Syntax Errors" }
        return transform(root)
    }
}
