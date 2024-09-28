package text_rdt

import scala.collection.mutable

object CausalBroadcast {
  final val ONE: Integer = 1
}

final case class CausalBroadcast[MSG](replicaId: RID) {

  // you can override this function if you want
  def appendMessage(
      buffer: mutable.ArrayBuffer[MSG],
      messageToAppend: MSG
  ): Unit = {
    buffer.addOne(messageToAppend)
  }

  var causalState: mutable.HashMap[RID, Integer] =
    new mutable.HashMap(2, mutable.HashMap.defaultLoadFactor)
  val _ = causalState.put(replicaId, CausalBroadcast.ONE)

   val cachedHeads: mutable.ArrayBuffer[CausalID] = mutable.ArrayBuffer.empty

  private val _history: mutable.ArrayBuffer[
    (CausalID, mutable.ArrayBuffer[MSG])
  ] = mutable.ArrayBuffer.empty


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
    (CausalID, mutable.ArrayBuffer[MSG])
  ] = {
    _history
      .to(Iterable)
      .filter(node =>
        !heads.exists(head => CausalID.partialOrder.lteq(node._1, head))
      )
      .map((causalId, messages) => (causalId, messages.clone()))
  }


  def addOneToHistory(msg: MSG): Unit = {
    if (_history.nonEmpty && _history.last._1 == causalState) {
      appendMessage(_history.last._2, msg)
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
      msg: (CausalID, mutable.ArrayBuffer[MSG])
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

  def syncFrom(other: CausalBroadcast[MSG], handleMessage: MSG => Unit): Unit = {
    val heads1 = this.cachedHeads
    val potentiallyNewer2 =
      other.elementsPotentiallyNewer(heads1)
    this.causalState = this.causalState.clone()
    other.causalState = other.causalState.clone()
    potentiallyNewer2.foreach(entry => {
      deliveringRemote(
        entry, handleMessage
      )
    })
    this.tick()
    other.tick()
  }

  def deliveringRemote(
      entry: (
          CausalID,
          mutable.ArrayBuffer[MSG]
      ),
      handleMessage: MSG => Unit
  ): Unit = {
    entry._1
      .foreachEntry((rid, counter) => {
        this.causalState
          .update(
            rid,
            Math.max(
              this.causalState.getOrElse(rid, CausalID.ZERO),
              counter
            )
          )
      })

    this.addToHistory(entry)

    entry._2.foreach(msg => handleMessage(msg)) // TODO test again whether the compiler can detect this with entry._2.foreach(msg => handleMessage)
  }
}
