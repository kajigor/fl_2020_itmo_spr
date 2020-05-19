package grammar.model

sealed class Symbol {
    data class Terminal(override val text: String) : Symbol() {
        override fun toString(): String = "'$text'"
    }

    data class NonTerminal(val name: String) : Symbol() {
        override fun toString(): String = "<$name>"

        override val text: String get() = name
    }

    abstract val text: String

    companion object {
        val EOF = Terminal("$")
        val EMPTY = Terminal("")
    }
}
