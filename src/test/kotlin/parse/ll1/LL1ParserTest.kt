package parse.ll1

import grammar.GrammarParser
import grammar.GrammarParserImpl
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.BeforeEach

internal class LL1ParserTest {
    lateinit var grammarParser: GrammarParser

    @BeforeEach
    fun setUp() {
        grammarParser = GrammarParserImpl()
    }

    @Test
    fun parserMatchListSimple() {
        val filePath = "src/test/resources/parse/support/lund.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertTrue(llParser.match(listOf("a", "d")))
    }


    @Test
    fun parserMatchListSimple2() {
        val filePath = "src/test/resources/parse/ll1table/simple.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a")))
        assertTrue(llParser.match(listOf("a")))
    }

    @Test
    fun parserMatchListExpression() {
        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("(", "n")))
        assertTrue(llParser.match(listOf("(", "n", ")")))
        assertTrue(llParser.match(listOf("(", "n", "+", "n", ")", "*", "n")))
        assertFalse(llParser.match(listOf("(", "n", "+", "n", "*", "n")))
    }



    @Test
    fun parserMatchListNumber() {
        val filePath = "src/test/resources/grammar/natural.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("1", "1", "n")))
        assertTrue(llParser.match(listOf("0", "1", "2", "4", "4", "4")))
        assertTrue(llParser.match(listOf("1", "2", "3")))
        assertTrue(llParser.match(listOf("0", "1", "2", "3", "0", "6", "8", "9")))
        assertFalse(llParser.match(listOf("0", ".", "1")))
    }


    @Test
    fun parserMatchPlus() {
        val filePath = "src/test/resources/parse/ll1table/plus.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a", "a", "b")))
        assertTrue(llParser.match(listOf("a", "a", "a", "a", "a", "a")))
        assertFalse(llParser.match(listOf()))
    }


    @Test
    fun parserMatchKleene() {
        val filePath = "src/test/resources/parse/ll1table/kleene.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a", "a", "b")))
        assertTrue(llParser.match(listOf("a")))
        assertTrue(llParser.match(listOf("a", "a", "a", "a", "a", "a")))
        assertTrue(llParser.match(listOf()))
    }
}
