package parse.ll1

import grammar.GrammarParser
import grammar.GrammarParserImpl
import grammar.model.Symbol
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach

internal class FollowTableTest {
    private lateinit var parser: GrammarParser

    @BeforeEach
    fun setUp() {
        parser = GrammarParserImpl()
    }

    /*
    Пример из:
        http://fileadmin.cs.lth.se/cs/Education/EDAN65/2017/lectures/L05A.pdf
     */
    @Test
    fun calculateFollowTableSimple() {
        val filePath = "src/test/resources/parse/support/non_ll1_2.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val a = Symbol.Terminal("a")
        val c = Symbol.Terminal("c")
        val d = Symbol.Terminal("d")
        val followTable = supportTableCalculator.followTable
        val expected: FollowTable = mapOf(
            Symbol.NonTerminal("X") to setOf(a, c, d, Symbol.EMPTY),
            Symbol.NonTerminal("Y") to setOf(a, c, d, Symbol.EMPTY),
            Symbol.NonTerminal("Z") to setOf(Symbol.EOF)
        )
        assertEquals(expected, followTable)
    }

    /// https://neerc.ifmo.ru/wiki/index.php?title=Построение_FIRST_и_FOLLOW#.D0.9F.D1.80.D0.B8.D0.BC.D0.B5.D1.80
    @Test
    fun calculateFollowTableExpressionTest() {
        val e = Symbol.NonTerminal("E")
        val e1 = Symbol.NonTerminal("E'")
        val t = Symbol.NonTerminal("T")
        val t1 = Symbol.NonTerminal("T'")
        val f = Symbol.NonTerminal("F")
        val eps = Symbol.NonTerminal("eps")
        val rbr = Symbol.Terminal(")")
        val multiplication = Symbol.Terminal("*")
        val plus = Symbol.Terminal("+")

        val filePath = "src/test/resources/parse/support/expression.grammar"
        val cfGrammar = parser.parse(filePath)
        val grammar = convertToLL1Grammar(cfGrammar)
        val supportTableCalculator = SupportTableCalculatorImpl(grammar)
        val followTable = supportTableCalculator.followTable
        val expected: FollowTable = mapOf(
            e to setOf(Symbol.EOF, rbr),
            e1 to setOf(Symbol.EOF, rbr),
            t to setOf(plus, Symbol.EOF, rbr, Symbol.EMPTY),
            t1 to setOf(plus, Symbol.EOF, rbr, Symbol.EMPTY),
            f to setOf(multiplication, plus, Symbol.EOF, rbr, Symbol.EMPTY),
            eps to setOf(Symbol.EOF, plus, rbr, Symbol.EMPTY)
        )
        assertEquals(expected, followTable)
    }
}
