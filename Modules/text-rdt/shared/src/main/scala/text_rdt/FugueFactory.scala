package text_rdt

import scala.collection.mutable

trait FugueFactory {
  type ID
  type N
  type NC <: TreeNodey[N] { type ID = FugueFactory.this.ID }
  type F
  type MSG

  given treeNodeContext: NC

  def create(replicaId: RID): F

  def appendMessage(
      buffer: mutable.ArrayBuffer[MSG],
      messageToAppend: MSG
  ): Unit = {
    buffer.addOne(messageToAppend)
  }

  extension (factory: F) {

    def textWithDeleted(): Vector[Either[Char, Char]]

    def text(): String

    def atVisibleIndex(i: Int): N

    def visibleIndexOf(node: N): Int

    def createRootNode(): N

    def insert(
        id: ID | Null,
        value: Char,
        parent: N,
        side: Side
    ): MSG

    def insert(i: Int, x: Char): MSG

    def dupe(): F

    def delete(node: N): MSG

    def handleRemoteMessage(message: MSG, editor: Editory): Unit

    def get(id: ID | Null): N
  }
}
