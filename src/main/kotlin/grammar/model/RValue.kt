package grammar.model

data class RValue(val rvalue: List<Symbol>) {
    override fun toString(): String = rvalue.joinToString(" ") { it.toString() }
}

fun RValue.suffix(fromIndex: Int): List<Symbol> = rvalue.subList(fromIndex, rvalue.size)

fun RValue.prefix(toIndex: Int): List<Symbol> = rvalue.subList(0, toIndex)
