package parse.ll1

import grammar.model.Symbol
import grammar.model.prefix
import grammar.model.suffix


class SupportTableCalculatorImpl(grammar: LL1Grammar) : SupportTableCalculator {
    override val firstTable: FirstTable by lazy { calculateFirstTable(grammar) }

    override val followTable: FollowTable by lazy { calculateFollowTable(grammar) }

    override val nullableTable: Nullable by lazy { calculateNullable(grammar) }

    override fun isNullable(symbol: Symbol): Boolean = nullableTable[symbol] == true

    override fun firstFor(symbol: Symbol): Set<Symbol.Terminal> {
        return when (symbol) {
            is Symbol.Terminal -> setOf(symbol)
            is Symbol.NonTerminal -> firstTable[symbol] ?: setOf()
        }
    }

    override fun followFor(symbol: Symbol): Set<Symbol.Terminal> = followTable[symbol] ?: setOf()

    private fun canNullify(symbols: List<Symbol>): Boolean {
        return symbols.all { isNullable(it) }
    }

    private fun calculateFollowTable(grammar: LL1Grammar): FollowTable {
        val table: MutableMap<Symbol, Set<Symbol.Terminal>> = grammar.nonTerminals
            .map { it to setOf<Symbol.Terminal>() }
            .toMap()
            .toMutableMap()
        table[grammar.startNonTerminal] = setOf(Symbol.EOF)


        var isChanged = true

        while (isChanged) {

            isChanged = false
            for ((head, rValue) in grammar.rules) {
                for (offset in 0 until rValue.rvalue.size - 1) {
                    val symbol = rValue.rvalue[offset]
                    if (symbol is Symbol.NonTerminal) {
                        val symbolSuffix = rValue.suffix(offset + 1)
                        val firstSymbols = firstSymbols(symbolSuffix)
                        val symbolSet = (table[symbol] ?: setOf()).union(firstSymbols)
                        if (symbolSet != table[symbol]) {
                            isChanged = true
                            table[symbol] = symbolSet
                        }
                    }
                }

                for (offset in rValue.rvalue.indices.reversed()) {
                    val symbol = rValue.rvalue[offset]
                    val symbolSuffix = rValue.suffix(offset + 1)
                    if (symbol is Symbol.NonTerminal && canNullify(symbolSuffix)) {
                        val symbolSet = (table[symbol] ?: setOf()).union(table[head] ?: setOf())
                        if (symbolSet != table[symbol]) {
                            isChanged = true
                            table[symbol] = symbolSet
                        }
                    }
                }
            }
        }

        return table
            .mapNotNull { (key, value) ->
                val newKey = key as? Symbol.NonTerminal ?: return@mapNotNull null
                newKey to value
            }
            .toMap()
    }

    private fun calculateFirstTable(grammar: LL1Grammar): FirstTable {
        val table: MutableMap<Symbol, Set<Symbol.Terminal>> = mutableMapOf()

        for ((nonTerminal, rValue) in grammar.rules) {
            table[nonTerminal] = setOf()

            for (symbol in rValue.rvalue)
                if (symbol is Symbol.Terminal)
                    table[symbol] = setOf(symbol)
        }

        var isChanged = true

        while (isChanged) {
            isChanged = false
            for ((nonTerminal, rValue) in grammar.rules) {
                val firstSymbol = rValue.rvalue.firstOrNull() ?: continue
                for (offset in rValue.rvalue.indices) {
                    val symbolPrefix = rValue.prefix(offset)

                    if (canNullify(symbolPrefix)) {
                        var nonTerminalSet = table[nonTerminal] ?: setOf()
                        val firstSymbolSet = table[firstSymbol] ?: setOf()
                        nonTerminalSet = nonTerminalSet.union(firstSymbolSet)
                        if (table[nonTerminal] != nonTerminalSet) {
                            isChanged = true
                            table[nonTerminal] = nonTerminalSet
                        }
                    }
                }
            }
        }

        return table
            .mapNotNull { (key, value) ->
                val newKey = key as? Symbol.NonTerminal ?: return@mapNotNull null
                newKey to value
            }
            .toMap()
    }

    private fun calculateNullable(grammar: LL1Grammar): Nullable {
        val rules = grammar.rules.groupBy({ it.first }, { it.second })
        val container: MutableMap<Symbol, Boolean> = mutableMapOf()
        val notNullableTerminals = grammar.terminals.map { it to false }
        container.putAll(notNullableTerminals)
        container[Symbol.EMPTY] = true
        container.putAll(grammar.nonTerminals.map { it to false })

        var isChanged = true
        while (isChanged) {
            isChanged = false
            for ((nonTerminal, rValues) in rules) {
                val hasNullableRValue = rValues.any { rValue ->
                    rValue.rvalue.all { container[it] == true }
                }
                if (container[nonTerminal] != hasNullableRValue && hasNullableRValue) {
                    isChanged = true
                    container[nonTerminal] = hasNullableRValue
                }
            }
        }

        return container
            .mapNotNull { (key, value) ->
                val newKey = key as? Symbol.NonTerminal ?: return@mapNotNull null
                newKey to value
            }
            .toMap()
    }


    private fun firstSymbols(symbols: List<Symbol>): Set<Symbol.Terminal> {
        return symbols.flatMap { firstFor(it) }.toSet()
    }
}
