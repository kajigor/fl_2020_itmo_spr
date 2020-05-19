package parse.base

import grammar.model.Symbol

fun <T> iff(condition: Boolean, producer: () -> T): T? {
    return if (condition) producer() else null
}

sealed class ParseError(message: String) : Exception(message)
class RuleNotFound(topSymbol: Symbol.NonTerminal, supposedTerminal: Symbol.Terminal) :
    ParseError("Не найдено правила для ${topSymbol to supposedTerminal}")

class StackNotEmpty : ParseError("Стек не пуст")

class UnexpectedTerminal(topSymbol: Symbol.Terminal) : ParseError("Неожиданный терминал: $topSymbol")

class NotTheWholeStringProcessed(rest: List<String>) : ParseError("Осталась непустая строка: ${rest.joinToString()}")

class NonLL1Grammar : ParseError("Грамматика не LL(1)")
