package grammar.model


data class Alternative(val alternatives: List<RValue>) {
    override fun toString(): String = alternatives.joinToString(" | ") { it.toString() }
}
