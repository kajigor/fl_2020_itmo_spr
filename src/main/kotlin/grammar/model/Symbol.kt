package grammar.model

sealed class Symbol {
    data class Terminal(val text: String) : Symbol() {
        override fun toString(): String = "'$text'"
    }

    data class NonTerminal(val name: String) : Symbol() {
        override fun toString(): String = "<$name>"
    }

    companion object {
        val EOF = Terminal("$")
        val EMPTY = Terminal("")
    }
}
