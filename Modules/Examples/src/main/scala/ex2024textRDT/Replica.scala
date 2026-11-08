package ex2024textRDT

import rdts.time.{Causal, CausalBuffer, Dot, Dots}

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

  // ----------------------------------------------------------------------------------------------
  // replication via an external dissemination layer (e.g., channels.PlumtreeIO)
  // ----------------------------------------------------------------------------------------------

  type Ops = Vector[state.factoryContext.MSG]

  /** Causal delivery of remote operations; `applied` contains the dots of all local and remote operation batches. */
  var causalBuffer: CausalBuffer[Ops] = CausalBuffer()

  /** Hand all local operations since the last call to `send` as one batch, together with their causal predecessors.
    * `send` returns the dot the dissemination layer assigned to the batch (`None` if it was not sent).
    */
  def broadcast(send: Causal[Ops] => Option[Dot]): Unit =
    if state.outgoing.nonEmpty then
        val delta = Causal(causalBuffer.applied, takeOutgoing())
        send(delta).foreach { dot =>
          val (next, ready) = causalBuffer.markApplied(Dots.single(dot))
          causalBuffer = next
          ready.foreach(applyRemoteUnchecked)
        }

  /** Receive a batch delivered by the dissemination layer under `dots`. Applies it once all predecessors are applied. */
  def receive(dots: Dots, delta: Causal[Ops]): Unit = {
    val (next, ready) = causalBuffer.receive(dots, delta)
    causalBuffer = next
    ready.foreach(applyRemoteUnchecked)
  }

  /** Remove and return all local operations since the last call. */
  def takeOutgoing(): Ops = {
    val ops = state.outgoing.toVector
    state.outgoing.clear()
    ops
  }

  /** Apply remote operations directly, without causal delivery. The caller must ensure causal order. */
  def applyRemoteUnchecked(ops: IterableOnce[state.factoryContext.MSG]): Unit =
    ops.iterator.foreach(msg => state.factoryContext.handleRemoteMessage(state.factory)(msg, editor))

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
}
