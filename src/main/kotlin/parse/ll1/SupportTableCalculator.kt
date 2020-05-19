package parse.ll1

import grammar.model.RValue
import grammar.model.Symbol

typealias FirstTable = Map<Symbol.NonTerminal, Set<Symbol.Terminal>>

typealias FollowTable = Map<Symbol.NonTerminal, Set<Symbol.Terminal>>

/// Словарь из нетерминалов во флаг, который говорит, может ли нетерминал зануляться
typealias Nullable = Map<Symbol.NonTerminal, Boolean>

/// LL1 таблица
typealias LL1Table = Map<Pair<Symbol.NonTerminal, Symbol.Terminal>, RValue>

interface SupportTableCalculator {
    val firstTable: FirstTable

    val followTable: FollowTable

    val nullableTable: Nullable

    val ll1Table: LL1Table

    fun isNullable(symbol: Symbol): Boolean

    fun firstFor(symbol: Symbol): Set<Symbol.Terminal>

    fun followFor(symbol: Symbol): Set<Symbol.Terminal>

    fun rValueFor(nonTerminal: Symbol.NonTerminal, terminal: Symbol.Terminal): RValue?
}
