package grammar

import antlr.CFGrammarParser
import grammar.model.Alternative
import grammar.model.CFGrammar
import grammar.model.RValue
import grammar.model.Symbol
import org.antlr.v4.runtime.tree.TerminalNode


internal fun transform(cfgrammar: CFGrammarParser.CfgrammarContext): CFGrammar {
    val rules = cfgrammar
        .cfrules()
        .cfrule()
        .map { transform(it) }
        .toMap()
    val terms = rules.values
        .flatMap { it.alternatives }
        .flatMap { it.rvalue }
        .mapNotNull { it as? Symbol.Term }
        .toSet()
    return CFGrammar(
        terms,
        rules.keys,
        rules
    )
}

internal fun transform(cfrule: CFGrammarParser.CfruleContext): Pair<Symbol.NonTerm, Alternative> {
    val nonTerm = transform(cfrule.nonterm())
    val rvalue = transform(cfrule.rvalue())
    return nonTerm to rvalue
}


internal fun transform(nonTerm: CFGrammarParser.NontermContext): Symbol.NonTerm {
    val text = nonTerm.id().ruleid().ID().text
    return Symbol.NonTerm(text)
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

internal fun transform(term: TerminalNode): Symbol.Term {
    return Symbol.Term(term.text)
}
