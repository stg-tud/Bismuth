package datatypes

import dag.{Event, HashDAG, KeyPair}

/** Op-based CRDT*/
case class Counter private(
                            hashDAG: HashDAG[Int]
                          ):
  lazy val value: Int = hashDAG.graph.map(
    (k, _) => k.content match {
      case Some(c) =>
        if c.isInstanceOf[Int] then c
        else 0
      case _   => 0
    }
  ).sum

  def inc: (Counter, Event[Int]) = add(1)

  def dec: (Counter, Event[Int]) = add(-1)

  def add(amount: Int): (Counter, Event[Int]) =
    val event = hashDAG.generator(amount)

    (Counter(hashDAG.effector(event)), event)

  def receiveEvent(event: Event[Int]): Counter =
    Counter(hashDAG.effector(event))


object Counter:
  def apply(): Counter =
    val authorKeyPair = KeyPair(Array.empty, Array.empty)
    new Counter(HashDAG[Int](authorKeyPair))





