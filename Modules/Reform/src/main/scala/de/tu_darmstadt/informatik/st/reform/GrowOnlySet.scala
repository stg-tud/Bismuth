package de.tu_darmstadt.informatik.st.reform

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.*

case class GrowOnlySet[A](set: Set[A]) {

  def add(value: A): GrowOnlySet[A] =
    GrowOnlySet(set + value)

  def union(other: GrowOnlySet[A]): GrowOnlySet[A] =
    GrowOnlySet(set.union(other.set))
}

object GrowOnlySet {

  def empty[A]: GrowOnlySet[A] = GrowOnlySet(Set.empty)

  given stringCodec: JsonValueCodec[GrowOnlySet[String]] = JsonCodecMaker.make

  given bottom[E]: Bottom[GrowOnlySet[E]] = new {
    override def empty: GrowOnlySet[E] = GrowOnlySet.empty
  }

  given GrowOnlySetLattice: [A] => Lattice[GrowOnlySet[A]] = Lattice.derived

  given [A]: Decompose[GrowOnlySet[A]] = a => List(a)
}
