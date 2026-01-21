package rdts.base

import rdts.base.Lattice
import rdts.time.Dots

import scala.annotation.targetName
import scala.compiletime.summonAll
import scala.deriving.Mirror

trait Historized[T] {

  /** get all redundant deltas from the buffer
    * @param delta the new delta
    * @param bufferedDelta a previously applied delta from the delta buffer
    * @return all dots (ids of deltas) that are subsumed by the new delta
    */
  def isRedundant(delta: T, bufferedDelta: T): Boolean

  extension (delta: T) {

    @targetName("isRedundantInfix")
    inline def isRedundant(butteredDelta: T): Boolean = Historized.this.isRedundant(delta, butteredDelta)

  }

}

object Historized {

  case class MetaDelta[T](id: Dots, delta: T, redundantDots: Dots) {
    def getAllDots: Dots = id.union(redundantDots)
  }

  object MetaDelta {

    def apply[T](ids: Dots, delta: T): MetaDelta[T] = MetaDelta(ids, delta, Dots.empty)

    extension [T](metaDeltas: Iterable[MetaDelta[T]]) {
      inline def getAllDots: Dots =
        metaDeltas.foldLeft(Dots.empty)((dots, metaDelta) => dots.union(metaDelta.id.union(metaDelta.redundantDots)))

      inline def mapDeltas[A](f: T => A): Iterable[MetaDelta[A]] =
        metaDeltas.map(bufferedDelta => bufferedDelta.copy(delta = f(bufferedDelta.delta)))

      inline def getRedundantDeltas(delta: T)(using Historized[T]): Dots =
        metaDeltas.foldLeft(Dots.empty)((dots, bufferedDelta) =>
            val redundantDeltas =
              if delta.isRedundant(bufferedDelta.delta) then bufferedDelta.getAllDots else Dots.empty
            dots.union(redundantDeltas)
        )
    }
  }

  given subsumption[T: Lattice]: Historized[T] = (delta, bufferedDelta) => delta `subsumes` bufferedDelta

  inline def derived[T <: Product: {Mirror.ProductOf}]: Historized[T] = productHistorized[T]

  inline def productHistorized[T <: Product](using pm: Mirror.ProductOf[T]): Historized[T] = {
    val historizables: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Historized]]
    new ProductHistorized[T](historizables, pm, valueOf[pm.MirroredLabel])
  }

  class ProductHistorized[T <: Product](
      historizables: Tuple,
      pm: Mirror.ProductOf[T],
      label: String
  ) extends Historized[T] {

    override def toString: String = s"ProductHistorized[${label}]"

    private def hist(i: Int): Historized[Any] = historizables.productElement(i).asInstanceOf[Historized[Any]]

    override def isRedundant(delta: T, bufferedDelta: T): Boolean = {
      Range(0, historizables.productArity).forall { i =>
        hist(i).isRedundant(delta.productElement(i), bufferedDelta.productElement(i))
      }
    }
  }

}
