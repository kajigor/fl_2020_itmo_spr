package grammar.model

sealed class Symbol {
    data class Terminal(val text: String) : Symbol() {
        override fun toString(): String = text
    }

    data class NonTerminal(val name: String) : Symbol() {
        override fun toString(): String = "<$name>"
    }
}

data class RValue(val rvalue: List<Symbol>) {
    override fun toString(): String = rvalue.joinToString(" ") { it.toString() }
}

data class Alternative(val alternatives: List<RValue>) {
    override fun toString(): String = alternatives.joinToString(" | ") { it.toString() }
}

data class CFGrammar(
    val terminals: Set<Symbol.Terminal>,
    val nonTerminals: Set<Symbol.NonTerminal>,
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

