package parse.ll1

import grammar.model.RValue
import grammar.model.Symbol
import grammar.model.prefix
import grammar.model.suffix
import parse.base.NonLL1Grammar

fun supportTableCalculator(grammar: LL1Grammar): SupportTableCalculator = SupportTableCalculatorImpl(grammar)

class SupportTableCalculatorImpl(grammar: LL1Grammar) : SupportTableCalculator {
    override val firstTable: FirstTable by lazy { calculateFirstTable(grammar) }

    override val followTable: FollowTable by lazy { calculateFollowTable(grammar) }

    override val nullableTable: Nullable by lazy { calculateNullable(grammar) }

    override val ll1Table: LL1Table by lazy { calculateLL1Table(grammar) }

    override fun isNullable(symbol: Symbol): Boolean = nullableTable[symbol] == true


    override fun firstFor(symbol: Symbol): Set<Symbol.Terminal> {
        return when (symbol) {
            is Symbol.Terminal -> setOf(symbol)
            is Symbol.NonTerminal -> firstTable[symbol] ?: setOf()
        }
    }

    override fun followFor(symbol: Symbol): Set<Symbol.Terminal> = followTable[symbol] ?: setOf()

    override fun rValueFor(nonTerminal: Symbol.NonTerminal, terminal: Symbol.Terminal): RValue? {
        return ll1Table[nonTerminal to terminal]
    }

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
                for (offset in 0 until rValue.size - 1) {
                    val symbol = rValue[offset]
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

                for (offset in rValue.indices.reversed()) {
                    val symbol = rValue[offset]
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

            for (symbol in rValue)
                if (symbol is Symbol.Terminal)
                    table[symbol] = setOf(symbol)
        }

        var isChanged = true

        while (isChanged) {
            isChanged = false
            for ((nonTerminal, rValue) in grammar.rules) {
                val firstSymbol = rValue.firstOrNull() ?: continue
                for (offset in rValue.indices) {
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
                    rValue.all { container[it] == true }
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


    private fun calculateLL1Table(grammar: LL1Grammar): LL1Table {
        val table: MutableMap<Pair<Symbol.NonTerminal, Symbol.Terminal>, RValue> = mutableMapOf()

        for ((nonTerminal, rValue) in grammar.rules) {
            val firstSet = firstSymbols(rValue)
            for (terminal in firstSet.minus(Symbol.EMPTY)) {
                check(table[nonTerminal to terminal], rValue)
                table[nonTerminal to terminal] = rValue
            }

            if (Symbol.EMPTY in firstSet) {
                val followSet = followSymbols(nonTerminal)
                for (terminal in followSet) {
                    check(table[nonTerminal to terminal], rValue)
                    table[nonTerminal to terminal] = rValue
                }
                if (Symbol.EOF in followSet) {
                    check(table[nonTerminal to Symbol.EOF], rValue)
                    table[nonTerminal to Symbol.EOF] = rValue
                }
            }
        }
        return table
    }

    private fun firstSymbols(symbols: List<Symbol>): Set<Symbol.Terminal> {
        var symbolsSet = setOf<Symbol.Terminal>()

        for (symbol in symbols) {
            symbolsSet = symbolsSet.union(firstFor(symbol))
            if (!isNullable(symbol))
                break
        }
        return symbolsSet
    }

    private fun followSymbols(symbol: Symbol.NonTerminal): Set<Symbol.Terminal> {
        return followTable[symbol] ?: setOf()
    }

    private fun check(rValue: RValue?, newRValue: RValue) {
        if (rValue != null && rValue != newRValue)
            throw NonLL1Grammar()
    }
}
