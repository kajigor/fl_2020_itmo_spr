package grammar

import grammar.model.CFGrammar
import grammar.model.Symbol
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*
import java.io.File

internal class GrammarParserImplTest {
    private lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }

    @Test
    fun parseTestNumber() {
        val filePath = "src/test/resources/grammar/natural.grammar"
        val grammar = parser.parse(filePath)

        val digits = (0..9).map { Symbol.Terminal("$it") }.toSet()
        assertEquals(digits.plus(Symbol.EMPTY), grammar.terminals)

        assertEquals(
            listOf("natural", "natural'", "digit", "eps").map { Symbol.NonTerminal(it) }.toSet(),
            grammar.nonTerminals
        )
        assertEquals(4, grammar.rules.size)

        val testContent = getFileContent(filePath)
        assertEquals(testContent, getGrammarRulesString(grammar))
    }

    @Test
    fun parseTestIdentifier() {
        val filePath = "src/test/resources/grammar/identifier.grammar"
        val grammar = parser.parse(filePath)
        val testContent = getFileContent(filePath)
        assertEquals(testContent, getGrammarRulesString(grammar))
    }

    @Test
    fun parseTestExpression() {
        val filePath = "src/test/resources/parse/support/expression.grammar"
        val grammar = parser.parse(filePath)
        val testContent = getFileContent(filePath)
        assertEquals(testContent, getGrammarRulesString(grammar))
    }

    private fun getGrammarRulesString(grammar: CFGrammar): Set<String> {
        return grammar.rulesToString().split("\n").toSet()
    }

    private fun getFileContent(filename: String): Set<String> {
        return File(filename).readLines().map { it.trim() }.filter { it.isNotEmpty() }.toSet()
    }
}
