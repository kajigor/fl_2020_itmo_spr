package parse.base


data class ParseMatch(val tree: DerivationTree, val originalText: String, val error: ParseError? = null) {
    val isMatched: Boolean get() = error == null
}
