package parse.info

import grammar.model.Symbol
import parse.base.DerivationNode
import parse.base.DerivationRoot
import parse.base.DerivationTree

class LatexForestLLDerivationTreeFormatter : LLDerivationTreeFormatter {
    override fun format(tree: DerivationTree): String {
        return """
            \begin{forest} for tree={edge path={\noexpand\path[\forestoption{edge}] (\forestOve{\forestove{@parent}}{name}.parent anchor) -- +(0,-12pt)-| (\forestove{name}.child anchor)\forestoption{edge label};}}
            ${formatted(tree)}
            \end{forest}
        """.trimIndent()

    }

    private fun formatted(tree: DerivationTree): String {
        return when (tree) {
            is DerivationNode -> {
                return "[${formatted(tree.content)}${formatChildren(tree)}]"
            }
            is DerivationRoot -> {
                "[${tree.children.joinToString { formatted(it) }}]"
            }
        }
    }

    private fun formatChildren(node: DerivationNode): String {
        return if (node.children.isEmpty() && node.content is Symbol.NonTerminal) {
            "[${formatted(Symbol.EMPTY)}]"
        } else {
            node.children.joinToString { formatted(it) }
        }
    }
}
