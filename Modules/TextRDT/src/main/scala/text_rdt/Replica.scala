package text_rdt

import rdts.time.{Causal, CausalBuffer, Dot, Dots}

import scala.collection.mutable

final case class Replica[F <: FugueFactory](
    state: ReplicaState[F],
    var editor: Editory
) {

  def insert(i: Int, x: Char): Unit = {
    editor.localInsert(i, x)
    state.insert(i, x)
  }

  def delete(i: Int): Unit = {
    editor.localDelete(i)
    state.delete(i)
  }

  def sync(other: Replica[F]): Unit = {
    this.syncFrom(other.state)
    other.syncFrom(this.state)
  }

  def syncFrom(other: ReplicaState[F]): Unit =
    this.state.causalBroadcast.syncFrom(
      other.causalBroadcast.asInstanceOf[CausalBroadcast[Replica.this.state.factoryContext.MSG]],
      (causalId, msg) => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor)
    )

  def deliveringRemote(
      entry: (
          CausalID,
          mutable.ArrayBuffer[Replica.this.state.factoryContext.MSG]
      ),
  ): Unit =
    state.causalBroadcast.deliveringRemote(
      entry,
      (causalId, msg) => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor)
    )

  // ----------------------------------------------------------------------------------------------
  // replication via an external dissemination layer (e.g., channels.BroadcastIO)
  // requires a state with `collectOutgoing = true`
  // ----------------------------------------------------------------------------------------------

  type Ops = Vector[state.factoryContext.MSG]

  /** Causal delivery of remote operations; `applied` contains the dots of all local and remote operation batches. */
  var causalBuffer: CausalBuffer[Ops] = CausalBuffer()

  /** Hand all local operations since the last call to `send` as one batch, together with their causal predecessors.
    * `send` returns the dot the dissemination layer assigned to the batch (`None` if it was not sent).
    */
  def broadcast(send: Causal[Ops] => Option[Dot]): Unit =
    if state.outgoing.nonEmpty then
        val delta = Causal(causalBuffer.applied, state.outgoing.toVector)
        state.outgoing.clear()
        send(delta).foreach { dot =>
          val (next, ready) = causalBuffer.markApplied(Dots.single(dot))
          causalBuffer = next
          applyRemote(ready)
        }

  /** Receive a batch delivered by the dissemination layer under `dots`. Applies it once all predecessors are applied. */
  def receive(dots: Dots, delta: Causal[Ops]): Unit = {
    val (next, ready) = causalBuffer.receive(dots, delta)
    causalBuffer = next
    applyRemote(ready)
  }

  private def applyRemote(batches: List[Ops]): Unit =
    batches.foreach(_.foreach(msg => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor)))

  def text(): String =
    state.text()

  def tree(): String =
    pprint
      .copy(
        defaultIndent = 0,
        additionalHandlers = {
          case value: state.factoryContext.N => value.treeify()
        }: @annotation.nowarn(
          "msg=cannot be checked at runtime because it refers to an abstract type member or type parameter"
        )
      )
      .apply(state.rootTreeNode)
      .plainText
      .replace("\n", "")
      .nn
}

object Replica {

  def apply[F <: FugueFactory](state: ReplicaState[F]): Replica[F] =
    Replica[F](state, null.asInstanceOf[Editory])

  given algorithm[F <: FugueFactory]: CollaborativeTextEditingAlgorithm[Replica[F]] with {
    extension (replica: Replica[F]) {
      def insert(i: Int, x: Char): Unit = replica.insert(i, x)

      def delete(i: Int): Unit = replica.delete(i)

      def text(): String = replica.text()

      def sync(other: Replica[F]): Unit = replica.sync(other)

      def syncFrom(other: Replica[F]): Unit = replica.syncFrom(other.state)
    }
  }
}
