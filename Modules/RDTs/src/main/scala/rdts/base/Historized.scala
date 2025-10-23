package rdts.base

import rdts.base.Historized.MetaDelta
import rdts.base.Lattice
import rdts.time.Dots

import scala.annotation.targetName
import scala.compiletime.summonAll
import scala.deriving.Mirror

trait Historized[T] {

  /** get all redundant deltas from the buffer
    * @param delta the new delta
    * @param buffer a buffer containing previously applied deltas
    * @return all dots (ids of deltas) that are subsumed by the new delta
    */
  def getRedundantDeltas(delta: T, buffer: Iterable[MetaDelta[T]]): Dots

  extension (delta: T) {

    @targetName("getRedundantDeltasInfix")
    inline def getRedundantDeltas(butter: Iterable[MetaDelta[T]]): Dots =
      Historized.this.getRedundantDeltas(delta, butter)

  }

}

object Historized {

  case class MetaDelta[T](id: Dots, delta: T, redundantDots: Dots)

  object MetaDelta {
    
    def apply[T](ids: Dots, delta: T): MetaDelta[T] = MetaDelta(ids, delta, Dots.empty)
    
    extension [T](metaDeltas: Iterable[MetaDelta[T]]) {
      inline def getAllDots: Dots = metaDeltas.foldLeft(Dots.empty)((dots, metaDelta) => dots.union(metaDelta.id.union(metaDelta.redundantDots)))

      inline def mapDeltas[A](f: T => A): Iterable[MetaDelta[A]] =
        metaDeltas.map(bufferedDelta => bufferedDelta.copy(delta = f(bufferedDelta.delta)))
    }
  }

  given subsumption[T: Lattice]: Historized[T] = (delta: T, buffer: Iterable[MetaDelta[T]]) => {
    val redundant = buffer.filter(bufferedDelta => delta `subsumes` bufferedDelta.delta).getAllDots
    println(f"get redundant dots for subsumption:\n\tdelta: $delta\n\tbuffer: ${buffer.size} $buffer\n\tredundant: $redundant")
    redundant
  }

  inline def derived[T <: Product: {Mirror.ProductOf, Lattice}]: Historized[T] = productHistorized[T]

  inline def productHistorized[T <: Product: Lattice](using pm: Mirror.ProductOf[T]): Historized[T] = {
    val historizables: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Historized]]
    new ProductHistorized[T](historizables, pm, valueOf[pm.MirroredLabel])
  }

  class ProductHistorized[T <: Product: Historized](
      historizables: Tuple,
      pm: Mirror.ProductOf[T],
      label: String
  ) extends Historized[T] {

    override def toString: String = s"ProductHistorized[${label}]"

    private def hist(i: Int): Historized[Any] = historizables.productElement(i).asInstanceOf[Historized[Any]]

    override def getRedundantDeltas(delta: T, buffer: Iterable[MetaDelta[T]]): Dots = {
      println(s"get redundant dots for product:\n\tdelta: $delta\n\tbuffer: ${buffer.size} $buffer")

      inline def redundantDotsAtZero: Dots = hist(0).getRedundantDeltas(
        delta.productElement(0),
        buffer.map(bufferedDelta => bufferedDelta.copy(delta = bufferedDelta.delta.productElement(0)))
      )

      historizables.productArity match {
        case 0 => Dots.empty
        case 1 => redundantDotsAtZero
        case _ =>
          val redundant = Range(1, historizables.productArity).foldLeft(redundantDotsAtZero) { (dots, i) =>
            dots.intersect(hist(i).getRedundantDeltas(
              delta.productElement(i),
              buffer.mapDeltas(_.productElement(i))
            ))
          }
          println(f"-redundant: $redundant")
          redundant
      }
    }
  }

}
