package ex2024textRDT

import ex2024textRDT.Helper.myAssert

import scala.collection.mutable

final case class ReplicaState[F <: FugueFactory](
    replicaId: RID
)(using
    val factoryContext: F
) {
  val factory: factoryContext.F = factoryContext.create(replicaId)

  val rootTreeNode: factoryContext.N = factory.createRootNode()

  /** Local operations not yet handed to the dissemination layer (see [[Replica.broadcast]]). */
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

  def insert(i: Int, x: Char): Unit =
    outgoing.addOne(factory.insert(i, x))

  def delete(i: Int): Unit = {
    val treeNode = factory.atVisibleIndex(i)
    outgoing.addOne(factory.delete(treeNode))
  }
}
