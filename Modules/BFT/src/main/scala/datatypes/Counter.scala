package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class Counter (
    hashDAG: HashDAG[Int]
) extends Replica[Int, Counter]:

    // filter out all events from byzantine nodes as well as the root event (id == "0")
    lazy val value: Int = hashDAG.events.values.filter { event =>
      !hashDAG.autohrIsByzantine(event.author) && event.id != "0"
    }
      .foldLeft(0) { (acc, event) => acc + event.content.get }

    def inc: Counter = add(1)

    def dec: Counter = add(-1)

    def add(amount: Int): Counter =
      Counter(hashDAG.generateDelta(amount))

    def merge(other: Counter): Counter =
        // var newValue   = this.value
        val newHashDAG = this.hashDAG.merge(other.hashDAG)

        // for every event merged, add the event content to the counter's value
        /*for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) && !event.authorIsByzantine then {
        newValue += event.content.get
      }*/

        Counter(newHashDAG)

    def empty: Counter = Counter()

    def withHashDAG(hashDAG: HashDAG[Int]): Counter = this.copy(hashDAG = hashDAG)

    override def generateDelta(ids: List[String]): Counter =
      Counter(hashDAG.getDelta(ids))

object Counter:
    def apply(): Counter =
        val keyPair = Ed25519Util.generateNewKeyPair
        new Counter(HashDAG[Int](keyPair.getPublic, Some(keyPair.getPrivate)))
