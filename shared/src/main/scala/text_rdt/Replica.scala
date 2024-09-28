package text_rdt

import scala.collection.mutable

final case class Replica[F <: FugueFactory](
    state: ReplicaState[F],
    editor: Editory
) {

  def sync(other: Replica[F]): Unit = {
    this.syncFrom(other.state)
    other.syncFrom(this.state)
  }

  def syncFrom(other: ReplicaState[F]): Unit = {
    this.state.causalBroadcast.syncFrom(other.causalBroadcast.asInstanceOf[CausalBroadcast[Replica.this.state.factoryContext.MSG]], msg => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor))
  }

  def deliveringRemote(
      entry: (
          CausalID,
          mutable.ArrayBuffer[Replica.this.state.factoryContext.MSG]
      ),
  ): Unit = {
    state.causalBroadcast.deliveringRemote(entry, msg => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor))
  }

  def text(): String = {
    state.text()
  }

  def tree(): String = {
    pprint
      .copy(
        defaultIndent = 0,
        additionalHandlers = { 
          case value: state.factoryContext.N => value.treeify()
        }: @annotation.nowarn("msg=cannot be checked at runtime because it refers to an abstract type member or type parameter")
      )
      .apply(state.rootTreeNode)
      .plainText
      .replace("\n", "")
      .nn
  }
}
