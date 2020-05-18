package grammar

import grammar.model.CFGrammar
import grammar.model.Symbol
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*
import java.io.File

internal class GrammarParserImplTest {
    lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }

    @Test
    fun parseTestNumber() {
        val filePath = "src/test/resources/grammar/natural.grammar"
        val grammar = parser.parse(filePath)

        assertEquals((0..9).map { Symbol.Terminal("$it") }.toSet(), grammar.terminals)
        assertEquals(setOf(Symbol.NonTerminal("natural"), Symbol.NonTerminal("digit")), grammar.nonTerminals)
        assertEquals(2, grammar.rules.size)

        val testContent = getFileContent(filePath)
        assertEquals(testContent, getGrammarRulesString(grammar))
    }

    @Test
    fun parseTestCBS() {
        val filePath = "src/test/resources/grammar/cbs.grammar"
        val grammar = parser.parse(filePath)
        assertEquals(setOf(Symbol.Terminal("("), Symbol.Terminal(")"), Symbol.Terminal("")), grammar.terminals)
        assertEquals(setOf(Symbol.NonTerminal("eps"), Symbol.NonTerminal("cbs")), grammar.nonTerminals)
        assertEquals(2, grammar.rules.size)
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

    private fun getGrammarRulesString(grammar: CFGrammar): Set<String> {
        return grammar.rulesToString().split("\n").toSet()
    }

    private fun getFileContent(filename: String): Set<String> {
        return File(filename).readLines().map { it.trim() }.filter { it.isNotEmpty() }.toSet()
    }
}
