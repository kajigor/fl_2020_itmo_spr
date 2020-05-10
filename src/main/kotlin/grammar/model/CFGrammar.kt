package grammar.model

sealed class Symbol {
    data class Term(val text: String): Symbol()
    data class NonTerm(val name: String): Symbol()
}

data class RValue(val rvalue: List<Symbol>)
data class Alternative(val alternatives: List<RValue>)

data class CFGrammar(
    val terms: Set<Symbol.Term>,
    val nonTerms: Set<Symbol.NonTerm>,
    val rules: Map<Symbol.NonTerm, Alternative>
)
