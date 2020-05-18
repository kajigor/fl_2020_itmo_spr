package grammar.model

data class CFGrammar(
    val terminals: Set<Symbol.Terminal>,
    val nonTerminals: Set<Symbol.NonTerminal>,
    val startNonTerminal: Symbol.NonTerminal,
    val rules: Map<Symbol.NonTerminal, Alternative>
) {
    override fun toString(): String =
        """
CFGrammar(
    terms=$terminals,
    nonTerms=$nonTerminals,
    rules = {
        ${rulesToString("\t\t")}
    }
)
""".trimIndent()

    fun rulesToString(indent: String = ""): String =
        rules.map { "${it.key} ::= ${it.value};" }.joinToString("\n" + indent)
}

