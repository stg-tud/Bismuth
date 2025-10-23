package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}

case class Counter private (
    value: Int,
    hashDAG: HashDAG[Int]
):

  def inc: Counter = add(1)

  def dec: Counter = add(-1)

  def add(amount: Int): Counter =
    Counter(0, hashDAG.generateDelta(amount))

  def merge(other: Counter): Counter =
    var newValue   = this.value
    val newHashDAG = this.hashDAG.merge(other.hashDAG)

    // for every event merged, add the event content to the counter's value
    for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) then {
        newValue += event.content.get
      }

    Counter(newValue, newHashDAG)

  def empty: Counter = Counter()

  def withHashDAG(hashDAG: HashDAG[Int]): Counter = this.copy(hashDAG = hashDAG)

object Counter:
  def apply(): Counter =
    val keyPair = Ed25519Util.generateNewKeyPair
    new Counter(0, HashDAG[Int](keyPair.getPublic, Some(keyPair.getPrivate)))
