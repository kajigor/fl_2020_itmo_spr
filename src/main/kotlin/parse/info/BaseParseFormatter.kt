package parse.info

import grammar.model.Symbol

interface BaseParseFormatter {
    fun formatted(symbol: Symbol): String {
        return when (symbol) {
            Symbol.EMPTY -> {
                "$\\varepsilon$"
            }
            Symbol.EOF -> {
                "\\$"
            }
            is Symbol.Terminal -> {
                "\\textit{${symbol.text}}"
            }
            is Symbol.NonTerminal -> {
                "\\textsl{\\textless ${symbol.text}\\textgreater}"
            }
        }
    }

    fun formatted(symbolSet: Set<Symbol>): String {
        return symbolSet.joinToString(", ") { formatted(it) }
    }
}
