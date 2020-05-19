import grammar.GrammarParser
import grammar.GrammarParserImpl
import parse.base.NonLL1Grammar
import parse.info.LatexForestLLDerivationTreeFormatter
import parse.info.LatexParseReportMaker
import parse.ll1.LL1Parser
import java.nio.file.Path


fun main(args: Array<String>) {
    check(args.size == 3) {
        """
        Usage:
        ./gradlew run --args="PATH_TO_GRAMMAR TEXT PATH_TO_REPORT_DIR"
    """.trimIndent()
    }

    val filePath = args[0]
    val string = args[1]
    val reportDir = args[2]
    val fileName = Path.of(filePath).fileName
    val grammarParser: GrammarParser = GrammarParserImpl()
    val grammar = grammarParser.parse(filePath)
    val parser = LL1Parser(grammar)
    try {
        val parseMatch = parser.match(string)
        val formatter = LatexForestLLDerivationTreeFormatter()
        val reportMaker = LatexParseReportMaker(grammar, parser.supportCalculator, parseMatch, formatter)

        reportMaker.makeReport(Path.of(reportDir, "$fileName.tex"))
        if (parseMatch.isMatched) {
            println("Успex!")
            println(grammar.rulesToString())
        } else {
            println("Ошибка!")
            println(parseMatch.error!!.message)
        }
    } catch (error: NonLL1Grammar) {
        println("ОШИБКА грамматики! ${error.message}")
    }
}
