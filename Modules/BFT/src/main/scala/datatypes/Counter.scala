package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}


case class Counter private (
    value: Int,
    hashDAG: HashDAG[Int],
    riblt: RIBLT[String]
) extends Replica[Int, Counter]:

  def inc: Counter = add(1)

  def dec: Counter = add(-1)

  def add(amount: Int): Counter =
    Counter(0, hashDAG.generateDelta(amount), RIBLT.empty)

  override def merge(other: Counter): Counter =
    var newValue         = this.value
    val newCausalContext = this.hashDAG.merge(other.hashDAG)

    // for every event merged, add the event content to the counter's value
    for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      if newCausalContext.contains(event) && !this.hashDAG.contains(event) then {
        this.riblt.addSymbol(event.id)
        newValue += event.content.get
      }

    Counter(newValue, newCausalContext, this.riblt)
  
  override def empty: Counter = Counter()

  override def withHashDAG(hashDAG: HashDAG[Int]): Counter = this.copy(hashDAG = hashDAG)

object Counter:
  def apply(): Counter =
    new Counter(0, HashDAG[Int](Ed25519Util.generateNewKeyPair), RIBLT.empty)
