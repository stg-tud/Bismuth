package test.rdts.simulatedNetworkTests.tests

import NetworkGenerators.*
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{GrowOnlyList, ReplicatedList}
import test.rdts.DataGenerator.ReplicatedListGen.makeRGA
import test.rdts.simulatedNetworkTests.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}

import scala.collection.mutable

class AntiEntropyBasicTest extends munit.ScalaCheckSuite {

  test("basic") {

    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[ReplicatedList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer[ReplicatedList[String]](ae)

    aec.mod(_.insert(0, "00")(using aec.replicaID))

    aec.mod(_.update(0, "UPD"))

    assertEquals(aec.data.toList, List("UPD"))

    aec.mod(_.insert(1, "100")(using aec.replicaID))

    assertEquals(aec.data.toList, List("UPD", "100"))

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.mod(_.insert(0, elem)(using aec.replicaID))
    // aec.modn(_.insertAll(using aec.replicaID)(0, lots))

    assertEquals(aec.data.toList, lots.reverse ::: List("UPD", "100"))

    aec.mod(_.insert(1, "b00")(using LocalUid.predefined("b")))

    assertEquals(aec.data.read(1), Some("b00"))

  }

  test("basic grow only list") {

    val network = new Network(0, 0, 0)

    val ae = new AntiEntropy[GrowOnlyList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer(ae)

    aec.mod(_.insertAt(0, "00"))

    assertEquals(aec.data.toList, List("00"))

    aec.mod(_.insertAt(1, "100"))

    assertEquals(aec.data.toList, List("00", "100"), aec.state)

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.mod(_.insertAt(0, elem))
    // aec.modn(_.insertAll(using aec.replicaID)(0, lots))

    assertEquals(aec.data.toList, lots.reverse ::: List("00", "100"))

    aec.mod(_.insertAt(1, "b00"))

    assertEquals(aec.data.read(1), Some("b00"))

  }

  test("specific property example") {

    val inserted: List[(Int, Int)]                       = List((0, 100))
    val removed: List[Int]                               = Nil
    val insertedAB: (List[(Int, Int)], List[(Int, Int)]) = (List((1, 4678)), Nil)
    val removedAB: (List[Int], List[Int])                = (Nil, List(0))
    val updatedAB: (List[(Int, Int)], List[(Int, Int)])  = (Nil, Nil)
    val network: Network = NetworkGenerator(0.4413085645880368, 0.382367536830158, 0.7030039323564119).make()

    val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

    val la0 = AntiEntropyContainer(aea)
    la0.applyDelta(Named(
      Uid.predefined(aea.replicaID),
      makeRGA(inserted, removed, LocalUid.predefined(aea.replicaID))
    ))
    network.startReliablePhase()
    AntiEntropy.sync(aea, aeb)
    network.endReliablePhase()
    val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

    val la1 = {
      val inserted = insertedAB._1.foldLeft(la0) {
        case (rga, (i, e)) => rga.mod(_.insert(i, e)(using rga.replicaID))
      }

      val deleted = removedAB._1.foldLeft(inserted) {
        case (rga, i) => rga.mod(_.delete(i))
      }

      updatedAB._1.foldLeft(deleted) {
        case (rga, (i, e)) => rga.mod(_.update(i, e))
      }
    }

    val lb1 = {
      val inserted = insertedAB._2.foldLeft(lb0) {
        case (rga, (i, e)) => rga.mod(_.insert(i, e)(using rga.replicaID))
      }

      val deleted = removedAB._2.foldLeft(inserted) {
        case (rga, i) => rga.mod(_.delete(i))
      }

      updatedAB._2.foldLeft(deleted) {
        case (rga, (i, e)) => rga.mod(_.update(i, e))
      }
    }

    val beforeA1: ReplicatedList[Int] = la1.state
    val beforeB1                      = lb1.state

    AntiEntropy.sync(aea, aeb)
    network.startReliablePhase()
    AntiEntropy.sync(aea, aeb)

    val la2 = la1.processReceivedDeltas()
    val lb2 = lb1.processReceivedDeltas()

    assertEquals(
      beforeA1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[ReplicatedList[Int]]),
      beforeA1
    )
    assertEquals(
      beforeB1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[ReplicatedList[Int]]),
      beforeB1
    )

    val directMergedState = beforeA1 `merge` beforeB1
    assertEquals(la2.state, directMergedState)
    assertEquals(lb2.state, directMergedState)

    assertEquals(
      la2.data.toList,
      lb2.data.toList,
      s"After synchronization messages were reliably exchanged all replicas should converge, but ${la2.data.toList} does not equal ${lb2.data.toList}\n  before A: $beforeA1\n  before B: $beforeB1\n  ${la2.state}\n  ${lb2.state}"
    )
  }

}
