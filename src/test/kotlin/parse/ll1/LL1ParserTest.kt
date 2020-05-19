package parse.ll1

import grammar.GrammarParser
import grammar.GrammarParserImpl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import parse.base.NonLL1Grammar

internal class LL1ParserTest {
    private lateinit var grammarParser: GrammarParser

    @BeforeEach
    fun setUp() {
        grammarParser = GrammarParserImpl()
    }

    @Test
    fun parserMatchListSimple2() {
        val filePath = "src/test/resources/parse/ll1table/simple.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a")).isMatched)
        assertTrue(llParser.match(listOf("a")).isMatched)
        assertTrue(llParser.match("a").isMatched)
    }


    @Test
    fun parserMatchListExpression() {
        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("(", "n")).isMatched)
        assertTrue(llParser.match(listOf("(", "n", ")")).isMatched)
        assertTrue(llParser.match(listOf("(", "n", "+", "n", ")", "*", "n")).isMatched)
        assertFalse(llParser.match(listOf("(", "n", "+", "n", "*", "n")).isMatched)
        assertTrue(llParser.match("(n)").isMatched)
        assertTrue(llParser.match("(n+n)*n+n+(n+n)").isMatched)
    }


    @Test
    fun parserMatchListNumber() {
        val filePath = "src/test/resources/grammar/natural.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("1", "1", "n")).isMatched)
        assertFalse(llParser.match("123n").isMatched)
        assertTrue(llParser.match(listOf("0", "1", "2", "4", "4", "4")).isMatched)
        assertTrue(llParser.match("3120945430819823423").isMatched)
        assertTrue(llParser.match(listOf("1", "2", "3")).isMatched)
        assertTrue(llParser.match(listOf("0", "1", "2", "3", "0", "6", "8", "9")).isMatched)
        assertFalse(llParser.match(listOf("0", ".", "1")).isMatched)
        assertFalse(llParser.match("0.23").isMatched)
    }


    @Test
    fun parserMatchPlus() {
        val filePath = "src/test/resources/parse/ll1table/plus.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a", "a", "b")).isMatched)
        assertTrue(llParser.match(listOf("a", "a", "a", "a", "a", "a")).isMatched)
        assertFalse(llParser.match("").isMatched)
    }


    @Test
    fun parserMatchKleene() {
        val filePath = "src/test/resources/parse/ll1table/kleene.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertFalse(llParser.match(listOf("a", "a", "a", "b")).isMatched)
        assertTrue(llParser.match(listOf("a")).isMatched)
        assertTrue(llParser.match(listOf("a", "a", "a", "a", "a", "a")).isMatched)
        assertTrue(llParser.match("").isMatched)
    }

    @Test
    fun parserMatchIdentifier() {
        val filePath = "src/test/resources/grammar/identifier.grammar"
        val cfGrammar = grammarParser.parse(filePath)
        val llParser = LL1Parser(cfGrammar)
        assertTrue(llParser.match("_id").isMatched)
        assertTrue(llParser.match("_3242id").isMatched)
        assertFalse(llParser.match("234_3242id").isMatched)
        assertTrue(llParser.match("___id_").isMatched)
        assertFalse(llParser.match("1___id_").isMatched)
    }


    @Test
    fun parserMatchListNonLL1_2() {
        val filePath = "src/test/resources/parse/support/non_ll1_2.grammar"
        val cfGrammar = grammarParser.parse(filePath)

        assertThrows(NonLL1Grammar::class.java) {
            val llParser = LL1Parser(cfGrammar)
            llParser.match(listOf("a", "d"))
            llParser.match("ad")
        }
    }

    @Test
    fun parserTestNonLL1Grammar() {
        val filePath = "src/test/resources/parse/ll1table/non_ll1.grammar"
        val cfGrammar = grammarParser.parse(filePath)

        assertThrows(NonLL1Grammar::class.java) {
            val llParser = LL1Parser(cfGrammar)
            llParser.match("(((())))(())")
        }
    }

    @Test
    fun parserTestNonLL1Grammar2() {
        val filePath = "src/test/resources/parse/ll1table/non_ll1_2.txt"
        val cfGrammar = grammarParser.parse(filePath)

        assertThrows(NonLL1Grammar::class.java) {
            val llParser = LL1Parser(cfGrammar)
            llParser.match("(((())))(())")
        }
    }
}
