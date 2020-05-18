package parse.ll1

import grammar.model.Symbol

typealias FirstTable = Map<Symbol.NonTerminal, Set<Symbol.Terminal>>

typealias FollowTable = Map<Symbol.NonTerminal, Set<Symbol.Terminal>>

/// Словарь из нетерминалов во флаг, который говорит, может ли нетерминал зануляться
typealias Nullable = Map<Symbol.NonTerminal, Boolean>

interface SupportTableCalculator {
    val firstTable: FirstTable

    val followTable: FollowTable

    val nullableTable: Nullable


    fun isNullable(symbol: Symbol): Boolean

    fun firstFor(symbol: Symbol): Set<Symbol.Terminal>

    fun followFor(symbol: Symbol): Set<Symbol.Terminal>
}
