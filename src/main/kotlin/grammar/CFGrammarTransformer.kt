package grammar

import antlr.CFGrammarParser
import grammar.model.Alternative
import grammar.model.CFGrammar
import grammar.model.RValue
import grammar.model.Symbol
import org.antlr.v4.runtime.tree.TerminalNode


internal fun transform(cfgrammar: CFGrammarParser.CfgrammarContext): CFGrammar {
    val rulesList = cfgrammar
        .cfrules()
        .cfrule()
        .map { transform(it) }

    val startNonTerminal = rulesList.first().first
    val rules = rulesList
        .groupBy { it.first }
        .mapValues { entry ->
            val rValues = entry.value.fold(setOf<RValue>()) { acc, alt ->
                acc.union(alt.second)
            }
            Alternative(rValues.toList())
        }

    val terms = rules.values
        .flatMap { it.alternatives }
        .flatMap { it.rvalue }
        .mapNotNull { it as? Symbol.Terminal }
        .toSet()
    return CFGrammar(
        terms,
        rules.keys,
        startNonTerminal,
        rules
    )
}

internal fun transform(cfrule: CFGrammarParser.CfruleContext): Pair<Symbol.NonTerminal, Alternative> {
    val nonTerm = transform(cfrule.nonterm())
    val rvalue = transform(cfrule.rvalue())
    return nonTerm to rvalue
}


internal fun transform(nonTerm: CFGrammarParser.NontermContext): Symbol.NonTerminal {
    val text = nonTerm.id().ruleid().ID().text
    return Symbol.NonTerminal(text)
}

internal fun transform(rvalue: CFGrammarParser.RvalueContext): Alternative {
    return transform(rvalue.alternatives())
}

internal fun transform(alternatives: CFGrammarParser.AlternativesContext): Alternative {
    return Alternative(alternatives.alternative().map { transform(it) })
}

internal fun transform(alternative: CFGrammarParser.AlternativeContext): RValue {
    val symbols = alternative.element().map { transform(it) }
    return RValue(symbols)
}

internal fun transform(element: CFGrammarParser.ElementContext): Symbol {
    val term = element.nonterm()?.let { transform(it) }
    val nonTerm = element.text()?.let { transform(it.STRING()) }
    val symbol = term ?: nonTerm
    return symbol!!
}

internal fun transform(term: TerminalNode): Symbol.Terminal {
    return Symbol.Terminal(term.text.trim('\''))
}
