package test.rdts.simulatedNetworkTests.tests

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import rdts.base.Uid
import rdts.datatypes.ReplicatedList
import test.rdts.DataGenerator.ReplicatedListGen.{makeRGA, given}
import test.rdts.simulatedNetworkTests.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}

import scala.collection.mutable

object RGAGenerators {

  def makeNet[E](rl: ReplicatedList[E]): AntiEntropyContainer[ReplicatedList[E]] =
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[ReplicatedList[E]]("a", network, mutable.Buffer())
      val aec     = AntiEntropyContainer[ReplicatedList[E]](ae)
      aec.applyDelta(Named(Uid.predefined("a"), rl))
      aec
}

class RGATest extends munit.ScalaCheckSuite {
  import RGAGenerators.{*}

  property("size, toList, read") {
    forAll { (rl: ReplicatedList[Int], readIdx: Int) =>
      val listInitial: List[Int] = rl.toList
      val rga                    = makeNet(rl)

      val rgaList = rga.data.toList

      assertEquals(listInitial, rgaList)

      assertEquals(rga.data.read(readIdx), listInitial.lift(readIdx))
    }

  }
  property("insert") {
    forAll { (rl: ReplicatedList[Int], insertIdx: Int, insertValue: Int) =>
      val rga = makeNet(rl)

      if insertIdx >= 0 && insertIdx < rga.data.size then {
        val inserted = rga.mod(_.insert(insertIdx, insertValue)(using rga.replicaID))

        assert(
          insertIdx < 0 || insertIdx > rga.data.size || inserted.data.read(
            insertIdx
          ).contains(insertValue),
          s"After inserting a value at $insertIdx, reading the rga at that index should return the inserted value but ${inserted.data.read(insertIdx)} does not contain $insertValue\n ${inserted.data.toList}\n  $rga\n  $inserted"
        )
        assert(
          (insertIdx >= 0 && insertIdx <= rga.data.size) || inserted.data.toList == rga.data.toList,
          s"Attempting to insertGL a value at an invalid index should not change the rga, but ${inserted.data.toList} does not equal ${rga.data.toList}"
        )
      }
    }

  }
  property("delete") {
    forAll { (rl: ReplicatedList[Int], deleteIdx: Int) =>
      if deleteIdx >= 0 && deleteIdx < rl.size then {
        val rga        = makeNet(rl)
        val sizebefore = rga.data.size
        val listbefore = rga.data.toList
        val deleted    = rga.mod(_.delete(deleteIdx))

        assert(
          deleteIdx < 0 || deleteIdx >= sizebefore || deleted.data.size == sizebefore - 1,
          s"After deleting a valid index the size of the rga should be reduced by 1, but ${deleted.data.size} does not equal ${rga.data.size} - 1"
        )
        assert(
          (deleteIdx >= 0 && deleteIdx < sizebefore) || deleted.data.toList == listbefore,
          s"Attempting to delete an invalid index should not change the rga, but ${deleted.data.toList} does not equal ${rga.data.toList}"
        )
      }
    }

  }
  property("update") {
    // Potentially many wasted executions ...
    forAll { (rl: ReplicatedList[Int], updateIdx: Int, updateValue: Int) =>
      if updateIdx >= 0 && updateIdx < rl.size then {

        val rllist      = rl.toList
        val rga         = makeNet(rl)
        val initiallist = rga.data.toList
        val updated     = rga.mod(_.update(updateIdx, updateValue))

        assert(
          updated.data.size == rga.data.size,
          s"update should not change the size of the rga, but ${updated.data.size} does not equal ${rga.data.size}"
        )

        assertEquals(rga.data.size, rga.data.toList.size)
        assert(
          updateIdx < 0 || updateIdx >= rga.data.size || updated.data.read(
            updateIdx
          ).contains(updateValue),
          s"After updating a valid index reading the rga at that index should return the updated value, but ${
              updated.data.read(
                updateIdx
              )
            } does not contain $updateValue ($updateIdx) \n  ${rga.data.toList}\n  $rllist\n  ${initiallist}\n  ${updated.state}\n  $rl"
        )
        assert(
          (updateIdx >= 0 && updateIdx < rga.data.size) || updated.data.toList == rga.data.toList,
          s"Attempting to update an invalid index should not change th rga, but ${updated.data.toList} does not equal ${rga.data.toList}"
        )
      }
    }

  }
  property("concurrent insert") {
    forAll {
      (inserted: List[(Int, Int)], removed: List[Int], n1: Int, e1: Int, n2: Int, e2: Int) =>
        val network = new Network(0, 0, 0)

        val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
        val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

        val la0 = AntiEntropyContainer(aea)
        la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
        AntiEntropy.sync(aea, aeb)
        val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

        val size = la0.data.size.toInt
        val idx1 = if size == 0 then 0 else math.floorMod(n1, size)
        val idx2 = if size == 0 then 0 else Math.floorMod(n2, size)

        val la1 = la0.mod(_.insert(idx1, e1)(using la0.replicaID))
        lb0.mod(_.insert(idx2, e2)(using lb0.replicaID))

        AntiEntropy.sync(aea, aeb)

        val la2 = la1.processReceivedDeltas()

        assert(
          idx1 < idx2 && la2.data.read(idx1).contains(e1) ||
          idx1 > idx2 && la2.data.read(idx1 + 1).contains(e1) ||
          idx1 == idx2 && (la2.data.read(idx1).contains(e1) || la2.data.read(idx1 + 1).contains(e1)),
          s"After synchronization $e1 was not found at its expected location in ${la2.data.toList}\n  ${la1} \n  ${lb0}"
        )
        assert(
          idx1 < idx2 && la2.data.read(idx2 + 1).contains(e2) ||
          idx1 > idx2 && la2.data.read(idx2).contains(e2) ||
          idx1 == idx2 && (la2.data.read(idx2).contains(e2) || la2.data.read(idx2 + 1).contains(e2)),
          s"After synchronization $e2 was not found at its expected location in ${la2.data.toList}"
        )
    }

  }
  property("concurrent delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, n1: Int, n2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx =
        if la0.data.size == 0 then 0 else math.floorMod(n, la0.data.size)

      val la1 = la0.mod(_.delete(idx))
      val lb1 = lb0.mod(_.delete(idx))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()
      val lb2 = lb1.processReceivedDeltas()

      assert(
        la2.data.toList == la1.data.toList,
        s"Concurrently deleting the same index twice should have the same result as deleting it once, but ${la2.data.toList} does not equal ${la1.data.toList}"
      )

      val size = la2.data.size.toInt
      val idx1 = if size == 0 then 0 else math.floorMod(n1, size)
      val idx2 = if size == 0 then 0 else math.floorMod(n2, size)

      val la3 = la2.mod(_.delete(idx1))
      lb2.mod(_.delete(idx2))

      AntiEntropy.sync(aea, aeb)

      val la4 = la3.processReceivedDeltas()

      val sequential =
        if idx1 > idx2 then {
          la2.mod(_.delete(idx1)).mod(_.delete(idx2))
        } else {
          la2.mod(_.delete(idx2)).mod(_.delete(idx1))
        }

      assert(
        idx1 == idx2 || la4.data.toList == sequential.data.toList,
        s"Concurrently deleting two different indices should have the same effect as sequential deletes, but ${la4.data.toList} does not equal ${sequential.data.toList}"
      )
    }

  }
}
