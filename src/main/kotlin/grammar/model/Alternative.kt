package grammar.model


data class Alternative(val alternatives: List<RValue>): List<RValue> by alternatives {
    override fun toString(): String = alternatives.joinToString(" | ") { it.toString() }
}
