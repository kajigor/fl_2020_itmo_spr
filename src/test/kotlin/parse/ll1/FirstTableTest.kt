package parse.ll1

import grammar.GrammarParser
import grammar.GrammarParserImpl
import grammar.model.RValue
import grammar.model.Symbol
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

internal class FirstTableTest {
    private lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }

    /*
https://neerc.ifmo.ru/wiki/index.php?title=LL(k)-грамматики,_множества_FIRST_и_FOLLOW#.D0.9F.D1.80.D0.B8.D0.BC.D0.B5.D1.80.D1.8B
     */
    @Test
    fun firstTableTest() {
        val lbr = Symbol.Terminal("(")
        val rbr = Symbol.Terminal(")")
        val a = Symbol.NonTerminal("A")
        val b = Symbol.NonTerminal("B")

        val grammar = LL1Grammar(
            setOf(lbr, rbr, Symbol.EMPTY),
            setOf(a, b),
            a,
            listOf(
                a to RValue(listOf(lbr, a, rbr, a)),
                a to RValue(listOf(Symbol.EMPTY)),
                b to RValue(listOf(b, b)),
                b to RValue(listOf(lbr, b, rbr)),
                b to RValue(listOf(Symbol.EMPTY))
            )
        )
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val firstTable = supportTableCalculator.firstTable

        val expected: FirstTable = mapOf(
            a to setOf(lbr, Symbol.EMPTY),
            b to setOf(lbr, Symbol.EMPTY)
        )
        assertEquals(expected, firstTable)
    }

    /*
https://neerc.ifmo.ru/wiki/index.php?title=LL(k)-грамматики,_множества_FIRST_и_FOLLOW#.D0.9F.D1.80.D0.B8.D0.BC.D0.B5.D1.80.D1.8B
    */
    @Test
    fun firstTableComplexTest() {
        val filePath = "src/test/resources/parse/support/dummy.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val firstTable = supportTableCalculator.firstTable

        val lbr = Symbol.Terminal("(")
        val eps = Symbol.NonTerminal("eps")
        val a = Symbol.NonTerminal("A")
        val b = Symbol.NonTerminal("B")
        val expected: FirstTable = mapOf(
            a to setOf(lbr, Symbol.EMPTY),
            b to setOf(lbr, Symbol.EMPTY),
            eps to setOf(Symbol.EMPTY)
        )
        assertEquals(expected, firstTable)
    }


    /*
    Example from:
    https://neerc.ifmo.ru/wiki/index.php?title=Построение_FIRST_и_FOLLOW#.D0.9A.D0.BE.D0.BD.D1.81.D1.82.D1.80.D1.83.D0.B8.D1.80.D0.BE.D0.B2.D0.B0.D0.BD.D0.B8.D0.B5_FIRST_.D0.B4.D0.BB.D1.8F_.D0.B0.D1.80.D0.B8.D1.84.D0.BC.D0.B5.D1.82.D0.B8.D1.87.D0.B5.D1.81.D0.BA.D0.B8.D1.85_.D0.B2.D1.8B.D1.80.D0.B0.D0.B6.D0.B5.D0.BD.D0.B8.D0.B9
     */
    @Test
    fun firstTableExpressionComplexTest() {
        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)

        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val firstTable = supportTableCalculator.firstTable
        val e = Symbol.NonTerminal("E")
        val e1 = Symbol.NonTerminal("E'")
        val t = Symbol.NonTerminal("T")
        val t1 = Symbol.NonTerminal("T'")
        val f = Symbol.NonTerminal("F")
        val eps = Symbol.NonTerminal("eps")
        val n = Symbol.Terminal("n")
        val lbr = Symbol.Terminal("(")
        val multiplication = Symbol.Terminal("*")
        val plus = Symbol.Terminal("+")


        val expected: FirstTable = mapOf(
            e to setOf(n, lbr),
            e1 to setOf(plus, Symbol.EMPTY),
            t to setOf(n, lbr),
            t1 to setOf(multiplication, Symbol.EMPTY),
            f to setOf(n, lbr),
            eps to setOf(Symbol.EMPTY)
        )
        assertEquals(expected, firstTable)
    }
}
