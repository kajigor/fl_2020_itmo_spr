package parse.base

import grammar.model.Symbol

sealed class DerivationTree {
    abstract val children: MutableList<DerivationNode>

    fun add(node: DerivationNode) {
        children.add(node)
    }
}

data class DerivationNode(
    val content: Symbol,
    override val children: MutableList<DerivationNode> = arrayListOf()
) : DerivationTree()


data class DerivationRoot(override val children: MutableList<DerivationNode> = arrayListOf()) : DerivationTree()
