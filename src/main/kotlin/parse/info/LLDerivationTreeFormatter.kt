package parse.info

import parse.base.DerivationTree

interface LLDerivationTreeFormatter: BaseParseFormatter {
    fun format(tree: DerivationTree): String
}
