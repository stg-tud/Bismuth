package text_rdt

import text_rdt.Helper.myAssert

// TODO FIXME generalize for any algorithm or extract the messaging part out of this
// probably extract messaging part first.

final case class ReplicaState[F <: FugueFactory](
    replicaId: RID
)(using
    val factoryContext: F
) {
  val causalBroadcast = CausalBroadcast[factoryContext.MSG](replicaId)

  val factory: factoryContext.F = factoryContext.create(replicaId)

  val rootTreeNode: factoryContext.N = factory.createRootNode()

  def text(): String = {
    val text = factory.text()
    for ((char, index) <- text.zipWithIndex) {
      myAssert(
        factory.atVisibleIndex(index).value().toString() == char.toString()
      )
    }
    text
  }

  def visibleIndexOf(node: factoryContext.N): Int = {
    factory.visibleIndexOf(node)
  }

  def get(id: factoryContext.ID): factoryContext.N = {
    factory.get(id)
  }

  def insert(i: Int, x: Char): Unit = {
    val message = factory.insert(i, x)

    causalBroadcast.addOneToHistory(
      message
    )
  }

  def delete(i: Int): Unit = {
    val treeNode = factory.atVisibleIndex(i)

    val msg = factory.delete(treeNode)

    causalBroadcast.addOneToHistory(msg)
  }
}
