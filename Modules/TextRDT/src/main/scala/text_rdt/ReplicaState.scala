package text_rdt

import text_rdt.Helper.myAssert

import scala.collection.mutable

final case class ReplicaState[F <: FugueFactory](
    replicaId: RID,
    /** Collect local operations in [[outgoing]], for use with an external dissemination layer (see [[Replica.broadcast]]). */
    collectOutgoing: Boolean = false,
)(using
    val factoryContext: F
) {
  val causalBroadcast = CausalBroadcast[factoryContext.MSG](replicaId)

  val factory: factoryContext.F = factoryContext.create(replicaId)

  val rootTreeNode: factoryContext.N = factory.createRootNode()

  /** Local operations not yet handed to an external dissemination layer (see [[Replica.broadcast]]). */
  val outgoing: mutable.ArrayBuffer[factoryContext.MSG] = mutable.ArrayBuffer.empty

  def text(): String = {
    val text = factory.text()
    for (char, index) <- text.zipWithIndex do
        myAssert(
          factory.atVisibleIndex(index).value().toString() == char.toString()
        )
    text
  }

  def visibleIndexOf(node: factoryContext.N): Int =
    factory.visibleIndexOf(node)

  def get(id: factoryContext.ID): factoryContext.N =
    factory.get(id)

  def insert(i: Int, x: Char): Unit = {
    val message = factory.insert(i, x)

    causalBroadcast.addOneToHistory(
      message
    )
    if collectOutgoing then outgoing.addOne(message)
  }

  def delete(i: Int): Unit = {
    val treeNode = factory.atVisibleIndex(i)

    val msg = factory.delete(treeNode)

    causalBroadcast.addOneToHistory(msg)
    if collectOutgoing then outgoing.addOne(msg)
  }
}
