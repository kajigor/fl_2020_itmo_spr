package parse.ll1

import grammar.model.CFGrammar
import parse.base.Parser


fun convertToLL1Grammar(grammar: CFGrammar): LL1Grammar {
    val rules = grammar
        .rules
        .flatMap { (key, value) ->
            value.alternatives.map { key to it }
        }
    return LL1Grammar(
        grammar.terminals,
        grammar.nonTerminals,
        grammar.startNonTerminal,
        rules
    )
}

class LL1Parser(cfGrammar: CFGrammar, private val supportCalculator: SupportTableCalculator) : Parser {
    private val grammar = convertToLL1Grammar(cfGrammar)

    override fun parse(string: String) {
    }
}
