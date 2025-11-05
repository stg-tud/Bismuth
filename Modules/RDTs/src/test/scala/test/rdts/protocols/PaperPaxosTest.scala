package test.rdts.protocols

import rdts.base.{Bottom, LocalUid}
import rdts.protocols.paper.Paxos as PaperPaxos
import rdts.protocols.{Participants, Paxos}
import rdts.time.Dots

class PaperPaxosTest extends munit.FunSuite {
  given Bottom[Int] with
     override def empty: Int = Int.MinValue

  given dots: Dots  = Dots.empty
  val id1: LocalUid = LocalUid.gen()
  val id2: LocalUid = LocalUid.gen()
  val id3: LocalUid = LocalUid.gen()

  given participants: Participants = Participants(Set(id1, id2, id3).map(_.uid))

  var emptyPaxosObject: Paxos[Int] = Paxos()

  test("Paxos for 3 participants without errors") {
    var a: Paxos[Int] = emptyPaxosObject

    a = a `merge` a.propose(1)(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.result, None)
    a = a `merge` a.upkeep()(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.result, Some(1))
  }
  test("Upkeep on empty") {
    var a: Paxos[Int] = emptyPaxosObject
    a.upkeep()(using id1)
  }
  test("Paperpaxos") {
    var a: PaperPaxos[Int] = PaperPaxos()

    a = a `merge` a.propose(1)(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.result, None)
    a = a `merge` a.upkeep()(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.result, Some(1))
  }
}
