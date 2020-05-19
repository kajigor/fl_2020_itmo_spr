package grammar.model


data class Alternative(val alternatives: List<RValue>): List<RValue> by alternatives {
    override fun toString(): String = toString { it.toString() }
    fun toString(formatter: (Symbol) -> String) = alternatives.joinToString(" | ") { it.toString(formatter) }
}
