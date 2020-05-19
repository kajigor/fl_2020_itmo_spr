package parse.ll1


import grammar.GrammarParser
import grammar.GrammarParserImpl
import grammar.model.RValue
import grammar.model.Symbol
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

internal class LL1TableTest {
    private lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }

    @Test
    fun ll1TableCalculateSimple() {
        val filePath = "src/test/resources/parse/ll1table/simple.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val ll1Table = supportTableCalculator.ll1Table
        val startNonTerm = Symbol.NonTerminal("S")
        val a = Symbol.Terminal("a")
        val expected: LL1Table = mapOf(
            (startNonTerm to a) to RValue(listOf(a))
        )
        assertEquals(expected, ll1Table)
    }


    @Test
    fun ll1TableCalculateExpression() {
        val e = Symbol.NonTerminal("E")
        val e1 = Symbol.NonTerminal("E'")
        val t = Symbol.NonTerminal("T")
        val t1 = Symbol.NonTerminal("T'")
        val f = Symbol.NonTerminal("F")
        val eps = Symbol.NonTerminal("eps")
        val rbr = Symbol.Terminal(")")
        val lbr = Symbol.Terminal("(")
        val multiplication = Symbol.Terminal("*")
        val plus = Symbol.Terminal("+")
        val n = Symbol.Terminal("n")

        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val ll1Table = supportTableCalculator.ll1Table

        val expected: LL1Table = mapOf(
            (e to n) to RValue(listOf(t, e1)),
            (e to lbr) to RValue(listOf(t, e1)),
            (e1 to plus) to RValue(listOf(plus, t, e1)),
            (e1 to Symbol.EOF) to RValue(listOf(eps)),
            (e1 to rbr) to RValue(listOf(eps)),
            (t to n) to RValue(listOf(f, t1)),
            (t to lbr) to RValue(listOf(f, t1)),
            (t1 to multiplication) to RValue(listOf(multiplication, f, t1)),
            (t1 to plus) to RValue(listOf(eps)),
            (t1 to rbr) to RValue(listOf(eps)),
            (t1 to Symbol.EOF) to RValue(listOf(eps)),
            (t1 to Symbol.EMPTY) to RValue(listOf(eps)),
            (f to n) to RValue(listOf(n)),
            (f to lbr) to RValue(listOf(lbr, e, rbr)),
            (eps to Symbol.EOF) to RValue(listOf(Symbol.EMPTY)),
            (eps to plus) to RValue(listOf(Symbol.EMPTY)),
            (eps to Symbol.EMPTY) to RValue(listOf(Symbol.EMPTY)),
            (eps to rbr) to RValue(listOf(Symbol.EMPTY))
        )
        assertEquals(expected, ll1Table)
    }
}
