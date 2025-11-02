package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class GrowOnlySet[T](
                     hashDAG: HashDAG[T]
                   ) extends Replica[T, GrowOnlySet[T]]:

  def add(element: T): GrowOnlySet[T] =
    GrowOnlySet(hashDAG.generateDelta(element))

  override def merge(other: GrowOnlySet[T]): GrowOnlySet[T] =
    val newHashDAG  = this.hashDAG.merge(other.hashDAG)

    GrowOnlySet(newHashDAG)

  def elements: Set[T] =
    hashDAG.events.values.filter(e => e.id == "0").map(e => e.content.get).toSet

  def contains(elem: T): Boolean = elements.contains(elem)

  def empty: GrowOnlySet[T] = GrowOnlySet()

  def withHashDAG(hashDAG: HashDAG[T]): GrowOnlySet[T] = this.copy(hashDAG = hashDAG)

  override def generateDelta(ids: List[String]): GrowOnlySet[T] =
    GrowOnlySet(hashDAG.getDelta(ids))

object GrowOnlySet:
  def apply[T](): GrowOnlySet[T] = {
    val keyPair = Ed25519Util.generateNewKeyPair
    new GrowOnlySet[T](HashDAG[T](keyPair.getPublic, Some(keyPair.getPrivate)))
  }
