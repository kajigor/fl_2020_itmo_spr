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

    fun rulesToString(indent: String = "", formatter: (Symbol) -> String = { it.toString() }): String =
        rules.map { "${formatter(it.key)} ::= ${it.value.toString(formatter)};" }.joinToString("\n" + indent)
}

