package grammar.model

data class RValue(val rvalue: List<Symbol>) : List<Symbol> by rvalue {
    override fun toString(): String = toString { it.toString() }
    fun toString(formatter: (Symbol) -> String): String = rvalue.joinToString(" ") { formatter(it) }

    val isEpsilon: Boolean get() = rvalue == listOf(Symbol.EMPTY)
}

fun RValue.suffix(fromIndex: Int): List<Symbol> = rvalue.subList(fromIndex, rvalue.size)

fun RValue.prefix(toIndex: Int): List<Symbol> = rvalue.subList(0, toIndex)
