package parse.ll1

import grammar.model.CFGrammar
import grammar.model.Symbol
import parse.base.*
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

private typealias StackContent = Pair<Symbol, DerivationTree>

class LL1Parser(cfGrammar: CFGrammar) : Parser {
    val grammar = convertToLL1Grammar(cfGrammar)
    val supportCalculator = supportTableCalculator(grammar)

    override fun match(tokens: List<String>): ParseMatch {
        val tokensList = tokens.plus(Symbol.EOF.text)
        val derivationTree: DerivationTree = DerivationRoot()
        val stack: ArrayDeque<StackContent> =
            ArrayDeque(listOf(grammar.startNonTerminal to derivationTree, Symbol.EOF to derivationTree))
        val tokenIterator = tokensList.iterator()
        var currentToken = tokenIterator.next()
        val parseMatch: (ParseError?) -> ParseMatch = { ParseMatch(derivationTree, tokens.joinToString(), it) }

        while (!stack.isEmpty()) {
            val (topSymbol, currentNode) = stack.first
            when {
                topSymbol is Symbol.Terminal && topSymbol.text == currentToken -> {
                    val newNode = DerivationNode(topSymbol)
                    currentNode.add(newNode)
                    stack.pop()
                    if (!tokenIterator.hasNext()) {
                        val error = iff(!stack.isEmpty()) { StackNotEmpty() }
                        return parseMatch(error)
                    }
                    currentToken = tokenIterator.next()
                }
                topSymbol is Symbol.Terminal -> return parseMatch(UnexpectedTerminal(topSymbol))
                topSymbol is Symbol.NonTerminal -> {
                    val newNode = DerivationNode(topSymbol)
                    currentNode.add(newNode)
                    val supposedTerminal = Symbol.Terminal(currentToken)
                    val rValue =
                        supportCalculator.rValueFor(topSymbol, supposedTerminal) ?: return parseMatch(
                            RuleNotFound(
                                topSymbol,
                                supposedTerminal
                            )
                        )

                    stack.pop()
                    if (!rValue.isEpsilon)
                        rValue.reversed().forEach { stack.push(it to newNode) }
                }
            }
        }

        val error = iff(tokenIterator.hasNext()) { NotTheWholeStringProcessed(tokenIterator.asSequence().toList()) }
        return parseMatch(error)
    }

    override fun match(content: String): ParseMatch {
        return match(content.map { "$it" })
    }
}
