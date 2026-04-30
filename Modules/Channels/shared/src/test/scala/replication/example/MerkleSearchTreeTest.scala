package replication.example

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Uid
import rdts.time.{CausalTime, Dot}
import replication.JsoniterCodecs.given
import replication.research.MerkleSearchTree
import test.rdts.DataGenerator.{ExampleData, given}

class MerkleSearchTreeTest extends munit.ScalaCheckSuite {

  given JsonValueCodec[ExampleData] = JsonCodecMaker.make

  given Ordering[Dot] = Ordering.by((dot: Dot) => (dot.time, Uid.unwrap(dot.place)))

  private def encode(value: ExampleData): Array[Byte] =
    com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(value)

  private def causalEntries(values: List[ExampleData]): Vector[(CausalTime, ExampleData)] =
    values.zipWithIndex.map { case (value, idx) =>
      CausalTime(idx.toLong, idx.toLong % 3, idx.toLong) -> value
    }.toVector

  private def dotEntries(values: List[ExampleData]): Vector[(Dot, ExampleData)] =
    values.zipWithIndex.map { case (value, idx) =>
      Dot(Uid.predefined(('a' + (idx % 3)).toChar.toString), idx.toLong) -> value
    }.toVector

  property("entry hash includes timestamp bytes") {
    forAll { (delta: ExampleData) =>
      MerkleSearchTree.entryHash(CausalTime(1, 0, 0), delta) != MerkleSearchTree.entryHash(CausalTime(2, 0, 0), delta)
    }
  }

  property("root hash is independent of insertion order") {
    forAll(Gen.listOf(implicitly[Arbitrary[ExampleData]].arbitrary)) { values =>
      val entries = causalEntries(values)
      val left    = MerkleSearchTree.fromEntries(entries, branchingFactor = 4)
      val right   = MerkleSearchTree.fromEntries(entries.reverse, branchingFactor = 4)

      left.rootHash == right.rootHash && left.entries.map(_.hash) == right.entries.map(_.hash)
    }
  }

  property("missingFrom returns exactly the missing causal-time entries") {
    forAll(Gen.listOf(implicitly[Arbitrary[ExampleData]].arbitrary), Gen.chooseNum(0, 20)) { (values, keep) =>
      val entries      = causalEntries(values)
      val local        = MerkleSearchTree.fromEntries(entries, branchingFactor = 4)
      val remoteInput  = entries.take(keep)
      val remote       = MerkleSearchTree.fromEntries(remoteInput, branchingFactor = 4)
      val expected     = local.entries.filterNot(entry => remote.contains(entry.hash)).map(_.hash).toSet
      val actual       = local.missingFrom(remote).map(_.hash).toSet

      actual == expected && remote.missingFrom(local).isEmpty
    }
  }

  test("insert remove and update keep the tree consistent and expose blocks") {
    val t0 = MerkleSearchTree.empty[CausalTime](branchingFactor = 3)
      .insert(CausalTime(1, 0, 0), ExampleData(Set("a")))
      .insert(CausalTime(2, 0, 0), ExampleData(Set("b")))
      .insert(CausalTime(3, 0, 0), ExampleData(Set("c")))

    val second = t0.entries(1)
    val updated = t0.update(second.hash, CausalTime(4, 0, 0), ExampleData(Set("bb")))
    val removed = updated.remove(t0.entries.head.hash)

    assertEquals(t0.size, 3)
    assertEquals(updated.size, 3)
    assertEquals(removed.size, 2)
    assert(!removed.contains(t0.entries.head.hash))
    assert(updated.entries.exists(_.encoded.sameElements(encode(ExampleData(Set("bb"))))))
    assert(removed.blocks.nonEmpty)
    assertEquals(removed.rootBlock.map(_.hash), removed.rootHash)
  }

  property("dot timestamps also work as ordering keys") {
    forAll(Gen.listOf(implicitly[Arbitrary[ExampleData]].arbitrary)) { values =>
      val entries = dotEntries(values)
      val local   = MerkleSearchTree.fromEntries(entries, branchingFactor = 3)
      val remote  = MerkleSearchTree.fromEntries(entries.filterNot(_._1.time % 2 == 0), branchingFactor = 3)

      val expected = local.entries.filterNot(entry => remote.contains(entry.hash)).map(_.hash).toSet
      val actual   = local.missingFrom(remote).map(_.hash).toSet

      actual == expected
    }
  }
}
