package parse.info

import grammar.model.CFGrammar
import grammar.model.Symbol
import parse.base.DerivationTree
import parse.base.ParseError
import parse.base.ParseMatch
import parse.ll1.*
import java.nio.file.Path

class LatexParseReportMaker(
    private val cfGrammar: CFGrammar,
    private val supportTableCalculator: SupportTableCalculator,
    private val matchInfo: ParseMatch,
    private val treeFormatter: LLDerivationTreeFormatter
) :
    ParseReportMaker {
    override fun makeReport(filePath: Path) {
        val content = makeContent()

        filePath.toFile().writeText(content)
    }

    private fun makeContent(): String {
        return """
\documentclass[10pt,twoside,a4paper]{memoir}
\usepackage{graphicx}
\usepackage{forest}
\usepackage{tikz-qtree}
\usepackage[english,russian]{babel}
\begin{document}
${formatGrammar(cfGrammar)}\\

\textsl{Исходная строка:} ${matchInfo.originalText}

${formatTree(matchInfo.tree)}

${formatError(matchInfo.error)}

${formatSupportTables(supportTableCalculator)}
\end{document}
""".trimIndent()
    }


    private fun formatTree(tree: DerivationTree): String {
        return """
\textbf{Дерево вывода:}

${treeFormatter.format(tree)}
        """.trimIndent()
    }

    private fun formatError(error: ParseError?): String {
        val parseError = error ?: return ""
        return """
Случилась ошибка! ${parseError.message ?: ""}
        """.trimIndent()
    }

    private fun formatGrammar(grammar: CFGrammar): String {
        return """
Грамматика:

${grammar.rulesToString("\n", this::formatted)}
        """.trimIndent()
    }

    private fun formatSupportTables(supportTableCalculator: SupportTableCalculator): String {
        return """
${formatNullableFirstFollowTable(
            supportTableCalculator.nullableTable,
            supportTableCalculator.firstTable,
            supportTableCalculator.followTable
        )}     

${formatLL1ParseTable(supportTableCalculator.ll1Table)}
""".trimIndent()
    }

    private fun formatNullableFirstFollowTable(
        nullable: Nullable,
        firstTable: FirstTable,
        followTable: FollowTable
    ): String {
        val rows = nullable.keys.union(firstTable.keys).union(followTable.keys).map(this::formatted)
        val nullableColumn = "Выводится ${formatted(Symbol.EMPTY)}"
        val firstColumn = "FIRST таблица"
        val followColumn = "FOLLOW таблица"
        val columns = listOf(nullableColumn, firstColumn, followColumn)
        val nullableContent = nullable
            .map { (formatted(it.key) to nullableColumn) to it.value.toString() }
        val firstContent = firstTable
            .map { (formatted(it.key) to firstColumn) to "\\{${formatted(it.value)}\\}" }
        val followContent = followTable
            .map { (formatted(it.key) to followColumn) to "\\{${formatted(it.value)}\\}" }
        val tableContent: Map<Pair<String, String>, String> =
            nullableContent.union(firstContent).union(followContent).toMap()
        return formatTable("Дополнительные таблицы:", rows, columns, tableContent)
    }

    private fun formatLL1ParseTable(parseTable: LL1Table): String {
        val rows = parseTable.keys.map { this.formatted(it.first) }
        val columns = parseTable.keys.map { this.formatted(it.second) }
        val tableContent = parseTable
            .map {
                val (nonTerminal, terminal) = it.key
                (formatted(nonTerminal) to formatted(terminal)) to it.value.toString(this::formatted)
            }.toMap()
        return formatTable("LL(1) таблица", rows, columns, tableContent)
    }

    private fun formatTable(
        title: String,
        rows: List<String>,
        columns: List<String>,
        content: Map<Pair<String, String>, String>
    ): String {
        return """
\begin{center}
\textsl{$title}

\begin{tabular}{ |c||${columns.indices.joinToString("|") { "c" }}| }
\hline
${(listOf("") + columns).joinToString(" & ")} \\
\hline\hline
${formatTableContent(rows, columns, content)}\\
\hline
\end{tabular}

\end{center}
        """.trimIndent()
    }

    private fun formatTableContent(
        rows: List<String>,
        columns: List<String>,
        content: Map<Pair<String, String>, String>
    ): String {
        return rows.joinToString("\\\\ \\hline\n") { rowName ->
            val columnsContent = columns.map { content[rowName to it] ?: "" }
            (listOf(rowName) + columnsContent).joinToString(" & ")
        }
    }
}
