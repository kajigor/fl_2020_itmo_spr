package parse.ll1

import grammar.model.CFGrammar
import grammar.model.Symbol
import parse.base.Parser
import java.util.ArrayDeque


fun convertToLL1Grammar(grammar: CFGrammar): LL1Grammar {
    val rules = grammar
        .rules
        .flatMap { (key, alternative) ->
            alternative.map { key to it }
        }
    return LL1Grammar(
        grammar.terminals,
        grammar.nonTerminals,
        grammar.startNonTerminal,
        rules
    )
}

class LL1Parser(cfGrammar: CFGrammar) : Parser {
    private val grammar = convertToLL1Grammar(cfGrammar)
    private val supportCalculator = supportTableCalculator(grammar)

    override fun match(tokens: List<String>): Boolean {
        if (tokens.isEmpty())
            return supportCalculator.isNullable(grammar.startNonTerminal)

        val tokensList = tokens + Symbol.EOF.text
        val stack: ArrayDeque<Symbol> = ArrayDeque(listOf(grammar.startNonTerminal, Symbol.EOF))
        val tokenIterator = tokensList.iterator()
        var currentToken = tokenIterator.next()

        while (!stack.isEmpty()) {
            val topStack = stack.first
            when {
                topStack.text == currentToken -> {
                    stack.pop()
                    if (!tokenIterator.hasNext())
                        return stack.isEmpty();
                    currentToken = tokenIterator.next()
                }
                topStack is Symbol.Terminal -> return false
                topStack is Symbol.NonTerminal -> {
                    val supposedTerminal = Symbol.Terminal(currentToken)
                    val rValue = supportCalculator.rValueFor(topStack, supposedTerminal) ?: return false
                    stack.pop()
                    if (!rValue.isEpsilon)
                        rValue.reversed().forEach(stack::push)
                }
            }
        }
        return !tokenIterator.hasNext()
    }
}
