package parse.ll1

import grammar.model.RValue
import grammar.model.Symbol

data class LL1Grammar(
    val terminals: Set<Symbol.Terminal>,
    val nonTerminals: Set<Symbol.NonTerminal>,
    val startNonTerminal: Symbol.NonTerminal,
    val rules: List<Pair<Symbol.NonTerminal, RValue>>
) {
    override fun toString(): String =
        """
LL1Grammar(
    terms=$terminals,
    nonTerms=$nonTerminals,
    rules = {
        ${rulesToString("\t\t")}
    }
)
""".trimIndent()

    private fun rulesToString(indent: String = ""): String =
        rules.joinToString("\n" + indent) { "${it.first} ::= ${it.second};" }
}
