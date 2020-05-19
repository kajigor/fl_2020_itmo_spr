package parse.ll1

import grammar.GrammarParser
import grammar.GrammarParserImpl
import grammar.model.Symbol
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach

internal class NullableTest {
    private lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }
    @Test
    fun calculateNullableTest() {
        val filePath = "src/test/resources/parse/support/non_ll1_2.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val nullables = supportTableCalculator.nullableTable
        val expected: Nullable = mapOf(
            Symbol.NonTerminal("X") to true,
            Symbol.NonTerminal("Y") to true,
            Symbol.NonTerminal("Z") to false
        )
        assertEquals(expected, nullables)
    }

    @Test
    fun calculateNullableTest2() {
        val e = Symbol.NonTerminal("E")
        val e1 = Symbol.NonTerminal("E'")
        val t = Symbol.NonTerminal("T")
        val t1 = Symbol.NonTerminal("T'")
        val f = Symbol.NonTerminal("F")
        val eps = Symbol.NonTerminal("eps")

        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val nullables = supportTableCalculator.nullableTable
        val expected: Nullable = mapOf(
            e to false,
            e1 to true,
            t to false,
            t1 to true,
            f to false,
            eps to true
        )
        assertEquals(expected, nullables)
    }

}
