package parse.base

interface Parser {
    fun match(tokens: List<String>): ParseMatch

    fun match(content: String): ParseMatch
}
