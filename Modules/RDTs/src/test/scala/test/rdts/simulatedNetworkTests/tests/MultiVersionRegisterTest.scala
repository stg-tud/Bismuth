package test.rdts.simulatedNetworkTests.tests

import NetworkGenerators.*
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Lattice
import rdts.datatypes.MultiVersionRegister
import test.rdts.simulatedNetworkTests.tools.{AntiEntropy, AntiEntropyContainer, Network}

import scala.collection.mutable
import scala.util.Random

object MVRegisterGenerators {

  def genMVRegister[A: Lattice](using
      a: Arbitrary[A],
  ): Gen[AntiEntropyContainer[MultiVersionRegister[A]]] =
    for
      values <- Gen.containerOf[List, A](a.arbitrary)
      nClear <- Gen.posNum[Short]
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[MultiVersionRegister[A]]("a", network, mutable.Buffer())

      val ops = Random.shuffle(values.indices ++ List.fill(nClear.toInt)(-1))

      ops.foldLeft(AntiEntropyContainer[MultiVersionRegister[A]](ae)) {
        case (r, -1) => r.map(_.clear())
        case (r, n)  => r.map(_.write(values(n))(using r.replicaID))
      }
    }

  given arbMVRegister[A: Lattice](using
      a: Arbitrary[A],
  ): Arbitrary[AntiEntropyContainer[MultiVersionRegister[A]]] =
    Arbitrary(genMVRegister)
}

class MultiVersionRegisterTest extends munit.ScalaCheckSuite {
  import MVRegisterGenerators.{*, given}

  given Lattice[Int] = math.max

  property("write") {
    forAll { (reg: AntiEntropyContainer[MultiVersionRegister[Int]], v: Int) =>
      val written = reg.mod(_.write(v)(using reg.replicaID))

      assert(
        written.data.read == Set(v),
        s"Sequentially writing a value should result in a singleton set containing that value, but ${written.data.read} does not equal ${Set(v)}"
      )
    }
  }
  property("clear") {
    forAll { (reg: AntiEntropyContainer[MultiVersionRegister[Int]]) =>
      val cleared = reg.mod(_.clear())

      assert(
        cleared.data.read.isEmpty,
        s"Clearing the register should result in an empty set, but ${cleared.data.read} is not empty"
      )
    }
  }
  property("concurrent write") {
    forAll { (vA: Int, vB: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

      val ra0 = AntiEntropyContainer[MultiVersionRegister[Int]](aea).mod(_.write(vA)(using aea.localUid))
      val rb0 = AntiEntropyContainer[MultiVersionRegister[Int]](aeb).mod(_.write(vB)(using aeb.localUid))

      AntiEntropy.sync(aea, aeb)

      val ra1 = ra0.processReceivedDeltas()
      val rb1 = rb0.processReceivedDeltas()

      assert(
        ra1.data.read == Set(vA, vB),
        s"Concurrently writing two values should result in a set containing both values, but ${ra1.data.read} does not equal ${Set(vA, vB)}"
      )
      assert(
        rb1.data.read == Set(vA, vB),
        s"Concurrently writing two values should result in a set containing both values, but ${rb1.data.read} does not equal ${Set(vA, vB)}"
      )
    }
  }
  property("concurrent write/clear") {
    forAll { (v: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

      val ra0 = AntiEntropyContainer[MultiVersionRegister[Int]](aea).mod(_.write(v)(using aea.localUid))
      val rb0 = AntiEntropyContainer[MultiVersionRegister[Int]](aeb).mod(_.clear())

      AntiEntropy.sync(aea, aeb)

      val ra1 = ra0.processReceivedDeltas()
      val rb1 = rb0.processReceivedDeltas()

      assert(
        ra1.data.read == Set(v),
        s"Writing a value should win over a concurrent clear, but ${ra1.data.read} does not equal ${Set(v)}"
      )
      assert(
        rb1.data.read == Set(v),
        s"Writing a value should win over a concurrent clear, but ${rb1.data.read} does not equal ${Set(v)}"
      )
    }
  }
  property("convergence") {
    forAll {
      (valuesA: List[Int], nClearA: Short, valuesB: List[Int], nClearB: Short, networkGen: NetworkGenerator) =>
        val network = networkGen.make()
        val aea     = new AntiEntropy[MultiVersionRegister[Int]]("a", network, mutable.Buffer("b"))
        val aeb     = new AntiEntropy[MultiVersionRegister[Int]]("b", network, mutable.Buffer("a"))

        val opsA = Random.shuffle(valuesA.indices ++ List.fill(nClearA.toInt)(-1))
        val opsB = Random.shuffle(valuesB.indices ++ List.fill(nClearB.toInt)(-1))

        val ra0 = opsA.foldLeft(AntiEntropyContainer[MultiVersionRegister[Int]](aea)) {
          case (r, -1) => r.mod(_.clear())
          case (r, n)  => r.mod(_.write(valuesA(n))(using r.replicaID))
        }
        val rb0 = opsB.foldLeft(AntiEntropyContainer[MultiVersionRegister[Int]](aeb)) {
          case (r, -1) => r.mod(_.clear())
          case (r, n)  => r.mod(_.write(valuesB(n))(using r.replicaID))
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val ra1 = ra0.processReceivedDeltas()
        val rb1 = rb0.processReceivedDeltas()

        assert(
          ra1.data.read == rb1.data.read,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${ra1.data.read} does not equal ${rb1.data.read}"
        )
    }
  }
}
