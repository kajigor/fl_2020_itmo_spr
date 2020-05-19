import grammar.GrammarParser
import grammar.GrammarParserImpl
import grammar.model.CFGrammar
import parse.base.NonLL1Grammar
import parse.info.LatexForestLLDerivationTreeFormatter
import parse.info.LatexParseReportMaker
import parse.ll1.LL1Parser
import java.nio.file.Path

private const val grammarParseOnlyFlag = "--parse_grammar_only"

private fun parse(string: String, grammarPath: String, reportDir: String) {
    val grammar = parseGrammar(grammarPath)
    val parser = LL1Parser(grammar)
    val fileName = Path.of(grammarPath).fileName
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

private fun parseGrammar(grammarPath: String): CFGrammar {
    val grammarParser: GrammarParser = GrammarParserImpl()
    return grammarParser.parse(grammarPath)
}

fun main(args: Array<String>) {
    when {
        grammarParseOnlyFlag in args && args.size == 2 -> {
            val filePath = args[1]
            val grammar = parseGrammar(filePath)
            println(grammar.rulesToString())
        }
        args.size == 1 -> {
            val filePath = args[0]
            val grammar = parseGrammar(filePath)
            println(grammar.rulesToString())
        }
        args.size == 3 -> { // парсинг. second task
            val filePath = args[0]
            val string = args[1]
            val reportDir = args[2]
            parse(string, filePath, reportDir)
        }
        else -> {
            println(
                """
Usage:
./gradlew run --args="PATH_TO_GRAMMAR TEXT PATH_TO_REPORT_DIR"
./gradlew run --args="[$grammarParseOnlyFlag] PATH_TO_GRAMMAR"            
                """.trimIndent()
            )
        }
    }

}
