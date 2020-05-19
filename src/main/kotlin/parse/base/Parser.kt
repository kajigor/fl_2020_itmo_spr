package parse.base

interface Parser {
    fun match(tokens: List<String>): Boolean

    fun match(content: String): Boolean
}
