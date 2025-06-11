package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.{*, given}

import scala.collection.mutable

object GSetGenerators {
  def genGSet[E: JsonValueCodec](using e: Arbitrary[E]): Gen[AntiEntropyContainer[Set[E]]] =
    for
      elements <- Gen.containerOf[List, E](e.arbitrary)
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[Set[E]]("a", network, mutable.Buffer())

      elements.foldLeft(AntiEntropyContainer[Set[E]](ae)) {
        case (set, e) => set.mod(_ => Set(e))
      }
    }

  given arbGSet[E: JsonValueCodec](using e: Arbitrary[E]): Arbitrary[AntiEntropyContainer[Set[E]]] =
    Arbitrary(genGSet)
}

class GSetTest extends munit.ScalaCheckSuite {
  import GSetGenerators.{*, given}

  given intCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  property("insert") {
    forAll { (set: AntiEntropyContainer[Set[Int]], e: Int) =>
      val setInserted = set.mod(_ => Set(e))

      assert(
        setInserted.data.contains(e),
        s"The set should contain an element after it is inserted, but ${setInserted.data} does not contain $e"
      )
    }
  }
  property("concurrent insert") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[Set[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[Set[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = AntiEntropyContainer[Set[Int]](aea).mod(_ => Set(e))
      val sb0 = AntiEntropyContainer[Set[Int]](aeb).mod(_ => Set(e))

      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      assert(
        sa1.data.contains(e),
        s"Concurrently inserting the same element should have the same effect as inserting it once, but ${sa1.data} does not contain $e"
      )
      assert(
        sb1.data.contains(e),
        s"Concurrently inserting the same element should have the same effect as inserting it once, but ${sb1.data} does not contain $e"
      )

      val sa2 = sa1.mod(_ => Set(e1))
      val sb2 = sb1.mod(_ => Set(e2))

      AntiEntropy.sync(aea, aeb)

      val sa3 = sa2.processReceivedDeltas()
      val sb3 = sb2.processReceivedDeltas()

      assert(
        Set(e1, e2).subsetOf(sa3.data),
        s"Concurrently inserting two elements should have the same effect as inserting them sequentially, but ${sa3.data} does not contain both $e1 and $e2"
      )
      assert(
        Set(e1, e2).subsetOf(sb3.data),
        s"Concurrently inserting two elements should have the same effect as inserting them sequentially, but ${sb3.data} does not contain both $e1 and $e2"
      )
    }
  }
  property("convergence") {
    forAll { (insertedA: List[Int], insertedB: List[Int], networkGen: NetworkGenerator) =>
      val network = networkGen.make()
      val aea     = new AntiEntropy[Set[Int]]("a", network, mutable.Buffer("b"))
      val aeb     = new AntiEntropy[Set[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = insertedA.foldLeft(AntiEntropyContainer[Set[Int]](aea)) {
        case (set, e) => set.mod(_ => Set(e))
      }
      val sb0 = insertedB.foldLeft(AntiEntropyContainer[Set[Int]](aeb)) {
        case (set, e) => set.mod(_ => Set(e))
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      assert(
        sa1.data == sb1.data,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${sa1.data} does not equal ${sb1.data}"
      )
    }
  }
}
