package datatypes

import crypto.Ed25519Util
import dag.{Event, HashDAG}

case class Counter private (
                             value: Int,
                             causalContext: HashDAG[Int]
                           ):
  
  def inc: Counter = add(1)

  def dec: Counter = add(-1)

  def add(amount: Int): Counter =
    Counter(0, causalContext.generateDelta(amount))

  def merge(other: Counter): Counter =
    var newValue = this.value
    val newCausalContext = this.causalContext.merge(other.causalContext)

    // for every event merged, add the event content to the counter's value
    for event <- this.causalContext.queue ++ other.causalContext.events.values ++ other.causalContext.queue do
      if newCausalContext.contains(event) && !this.causalContext.contains(event) then
        newValue += event.content.get

    Counter(newValue, newCausalContext)

object Counter:
  def apply(): Counter =
    new Counter(0, HashDAG[Int](Ed25519Util.generateNewKeyPair))
