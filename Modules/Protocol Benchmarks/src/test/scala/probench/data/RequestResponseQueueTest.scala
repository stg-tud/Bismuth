package probench.data

import probench.data.RequestResponseQueue.Req
import rdts.base.{Lattice, LocalUid}

import scala.util.Random

class RequestResponseQueueTest extends munit.ScalaCheckSuite {

  type CUT = RequestResponseQueue[String, String]

  def empty: CUT = RequestResponseQueue.empty

  test("add request works") {
    given LocalUid = LocalUid.predefined("id1")

    val queue         = empty
    val (_, reqDelta) = queue.request("one")
    val merged        = queue.merge(reqDelta)

    assertEquals(merged.requests.queryAllEntries.toList.head.value, "one")
  }

  test("add several requests") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty
    for n <- Range(0, 100) do {
      val (_, delta) = queue.request(n.toString)
      queue = queue.merge(delta)
    }
    assertEquals(queue.requestsSorted.length, 100)
    queue = queue.merge(queue.respond(queue.firstUnansweredRequest.get, "ok"))
    assertEquals(queue.requestsSorted.length, 99)
  }

  test("add requests merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val deltas = (0 to 10).map { i =>
      val (_, delta) = queue.request(f"req $i")
      queue = queue.merge(delta)
      delta
    }

    for _ <- 0 to 100 do
        assertEquals(Random.shuffle(deltas).foldLeft(empty)(Lattice.merge), queue)
  }

  test("respond to single request") {
    given LocalUid = LocalUid.predefined("id1")

    val queue         = empty
    val (_, reqDelta) = queue.request("one")
    val mergedQueue   = queue.merge(reqDelta)
    val request       = mergedQueue.requestsSorted.head
    val resDelta      = mergedQueue.respond(request, "1")
    val merged: CUT   = mergedQueue.merge(resDelta)

    assertEquals(merged.responseTo(request).map(_.value), Some("1"))
  }

  test("respond merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val reqDeltas = (0 to 10).map { i =>
      val (_, delta) = queue.request(f"req $i")
      queue = queue.merge(delta)
      delta
    }

    val resDeltas = (0 to 10).map { i =>
      val delta = queue.respond(queue.firstUnansweredRequest.get, f"res $i")
      queue = queue.merge(delta)
      delta
    }

    val allDeltas = reqDeltas ++ resDeltas

    for _ <- 0 to 100 do
        assertEquals(Random.shuffle(allDeltas).foldLeft(empty)(Lattice.merge), queue)
  }

  test("one uncompleted, one completed") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    def mod(f: CUT => CUT): CUT = {
      val delta = f(queue)
      queue = queue.merge(delta)
      delta
    }

    val req1                    = mod(_.request("one")._2)
    val req2                    = mod(_.request("two")._2)
    val requestOne: Req[String] = queue.requestsSorted.head
    val res1                    = mod(q => q.respond(q.requestsSorted.head, "1"))
    val res2                    = mod(q => q.respond(q.requestsSorted.head, "2"))

    // assertEquals(queue.requests.values.map(_.value).toList, List())
    assertEquals(queue.firstUnansweredRequest, None)
    assertEquals(queue.responses.queryAllEntries.map(_.value).toList, List("2"))

    val receive = mod(q => q.receive(requestOne.timestamp))

    assertEquals(queue.requestsSorted.map(_.value), List.empty)
    assertEquals(queue.responses.queryAllEntries.map(_.value).toSet, Set("2"))

    mod(q => q.respond(requestOne, "1")) // respond again

    assertEquals(queue.responses.queryAllEntries.map(_.value).toSet, Set("1"))
//    assertEquals(queue.responses.values.map(_.value).toList, List("2"))
  }

}
