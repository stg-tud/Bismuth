package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import rdts.base
import rdts.base.{Bottom, Decompose, LocalUid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.given

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

class ORMapTest extends munit.ScalaCheckSuite {
  given intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  given decompose[K, V: Decompose]: Decompose[ObserveRemoveMap[K, V]] = Decompose.atomic

  property("contains") {
    given LocalUid = base.LocalUid.predefined("test")
    given Bottom[Int] with
      def empty = Int.MinValue
    forAll { (entries: List[Int]) =>
      val orMap = entries.foldLeft(ObserveRemoveMap.empty[Int, Int]) { (curr, elem) =>
        curr.update(elem, elem)
      }
      orMap.entries.foreach { (k, v) =>
        assert(orMap.contains(k))
      }
    }
  }

  property("mutateKey/queryKey") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>

      val network = new Network(0, 0, 0)
      val aea     = new AntiEntropy[ObserveRemoveMap[Int, ReplicatedSet[Int]]]("a", network, mutable.Buffer())
      val aeb     = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer())

      val set = {
        val added: AntiEntropyContainer[ReplicatedSet[Int]] = add.foldLeft(AntiEntropyContainer(aeb)) {
          case (s, e) => s.mod(_.add(using s.replicaID)(e))
        }

        remove.foldLeft(added) {
          case (s, e) => s.mod(_.remove(e))
        }
      }

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, ReplicatedSet[Int]]](aea)) {
          case (m, e) =>
            m.mod(_.transformPlain(using m.replicaID)(k) { rs =>
              val before = rs.getOrElse(ReplicatedSet.empty[Int])
              Some(before `merge` before.add(using m.replicaID)(e))
            })
        }
        val res = remove.foldLeft(added) {
          case (m, e) =>
            m.mod(_.transformPlain(using aea.localUid)(k)(_.map(v => v.remove(e))))
        }
        res
      }

      val mapElements = map.data.get(k).getOrElse(ReplicatedSet.empty).elements

      assert(
        mapElements == set.data.elements,
        s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.data.elements}\n\t${map.state}\n\t${set.state}"
      )
    }
  }

  property("remove") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>
      val network = new Network(0, 0, 0)
      val aea     =
        new AntiEntropy[ObserveRemoveMap[Int, ReplicatedSet[Int]]]("a", network, mutable.Buffer())
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer())

      val empty = AntiEntropyContainer[ReplicatedSet[Int]](aeb)

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, ReplicatedSet[Int]]](aea)) {
          case (m, e) => m.mod(_.transformPlain(using aea.localUid)(k)(_.map(_.add(using m.replicaID)(e))))
        }

        remove.foldLeft(added) {
          case (m, e) => m.mod(_.transformPlain(using aea.localUid)(k)(_.map(_.remove(e))))
        }
      }

      val removed = map.mod(_.remove(k))

      val queryResult = removed.data.get(k).getOrElse(ReplicatedSet.empty).elements

      assertEquals(
        queryResult,
        empty.data.elements,
        s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.data.elements}"
      )
    }
  }

}
