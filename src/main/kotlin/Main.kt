import grammar.GrammarParser
import grammar.GrammarParserImpl


fun main(args: Array<String>) {
    check(args.size == 1) { "Should be only 1 argument – path to grammar" }

    val filePath = args[0]

    val grammarParser: GrammarParser = GrammarParserImpl()
    val grammar = grammarParser.parse(filePath)
    print(grammar)
}
