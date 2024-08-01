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
    val heads1 = this.state.cachedHeads
    val potentiallyNewer2 =
      other.elementsPotentiallyNewer(heads1)
    this.state.causalState = this.state.causalState.clone()
    other.causalState = other.causalState.clone()
    potentiallyNewer2.foreach(entry => {
      deliveringRemote(
        entry.asInstanceOf[
          (
              text_rdt.CausalID,
              scala.collection.mutable.ArrayBuffer[
                Replica.this.state.factoryContext.MSG
              ]
          )
        ]
      )
    })
    this.state.tick()
    other.tick()
  }

  def deliveringRemote(
      entry: (
          CausalID,
          mutable.ArrayBuffer[state.factoryContext.MSG]
      )
  ): Unit = {
    entry._1
      .foreachEntry((rid, counter) => {
        this.state.causalState
          .update(
            rid,
            Math.max(
              this.state.causalState.getOrElse(rid, CausalID.ZERO),
              counter
            )
          )
      })

    this.state.addToHistory(entry)

    entry._2.foreach(e =>
      state.factoryContext.handleRemoteMessage(state.factory)(e, editor)
    )
  }

  def text(): String = {
    state.text()
  }

  def tree(): String = {
    pprint
      .copy(
        defaultIndent = 0,
        additionalHandlers = { case value: state.factoryContext.N =>
          value.treeify()
        }
      )
      .apply(state.rootTreeNode)
      .plainText
      .replace("\n", "")
      .nn
  }
}
