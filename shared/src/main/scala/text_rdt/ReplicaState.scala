package text_rdt

import text_rdt.Helper.myAssert

import scala.collection.mutable

object ReplicaState {
  final val ONE: Integer = 1
}

final case class ReplicaState[F <: FugueFactory](
    replicaId: RID
)(using
    val factoryContext: F
) {
  val factory: factoryContext.F = factoryContext.create(replicaId)

  var causalState: mutable.HashMap[RID, Integer] =
    new mutable.HashMap(2, mutable.HashMap.defaultLoadFactor)
  val _ = causalState.put(replicaId, ReplicaState.ONE)

  val rootTreeNode: factoryContext.N = factory.createRootNode()
  val cachedHeads: mutable.ArrayBuffer[CausalID] = mutable.ArrayBuffer.empty

  private val _history: mutable.ArrayBuffer[
    (CausalID, mutable.ArrayBuffer[factoryContext.MSG])
  ] = mutable.ArrayBuffer.empty

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

  def calculatedHeads(): mutable.ArrayBuffer[CausalID] = {
    mutable.ArrayBuffer.from(
      _history
        .to(Iterable)
        .filter(outer =>
          !_history.exists(inner =>
            outer._1
              .ne(inner._1) && CausalID.partialOrder.lteq(outer._1, inner._1)
          )
        )
        .map(_._1)
    )
  }

  def elementsPotentiallyNewer(
      heads: mutable.ArrayBuffer[CausalID]
  ): Iterable[
    (CausalID, mutable.ArrayBuffer[factoryContext.MSG])
  ] = {
    _history
      .to(Iterable)
      .filter(node =>
        !heads.exists(head => CausalID.partialOrder.lteq(node._1, head))
      )
      .map((causalId, messages) => (causalId, messages.clone()))
  }

  def insert(i: Int, x: Char): Unit = {
    val message = factory.insert(i, x)

    addOneToHistory(
      message
    )
  }

  def delete(i: Int): Unit = {
    val treeNode = factory.atVisibleIndex(i)

    val deleted = treeNode.value() != null
    val msg = factory.delete(treeNode)

    addOneToHistory(msg)
  }

  private def addOneToHistory(msg: factoryContext.MSG): Unit = {
    if (_history.nonEmpty && _history.last._1 == causalState) {
      factoryContext.appendMessage(_history.last._2, msg)
    } else {
      cachedHeads
        .filterInPlace(cachedHead =>
          !CausalID.partialOrder
            .lteq(cachedHead, causalState)
        )
      cachedHeads.addOne(causalState)
      _history.addOne((causalState, mutable.ArrayBuffer(msg)))
    }
  }

  def addToHistory(
      msg: (CausalID, mutable.ArrayBuffer[factoryContext.MSG])
  ): Unit = {
    if (_history.lastOption.exists(p => p._1 == msg._1)) {
      _history.last._2.addAll(msg._2)
    } else {
      cachedHeads
        .filterInPlace(cachedHead =>
          !CausalID.partialOrder
            .lteq(cachedHead, msg._1)
        )
      cachedHeads.addOne(msg._1.clone())
      _history.addOne(msg)
    }
  }

  def tick(): Unit = {
    causalState
      .update(
        replicaId,
        causalState.getOrElse(replicaId, CausalID.ZERO) + 1
      )
    cachedHeads
      .filterInPlace(cachedHead =>
        !CausalID.partialOrder
          .lteq(cachedHead, causalState)
      )
    cachedHeads.addOne(causalState)
  }
}
