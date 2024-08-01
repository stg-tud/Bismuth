package text_rdt

import scala.collection.mutable
import upickle.default._

type CausalID = mutable.HashMap[RID, Integer]

implicit val fooReadWrite: ReadWriter[mutable.HashMap[RID, Integer]] =
  readwriter[Map[RID, Integer]]
    .bimap[mutable.HashMap[RID, Integer]](Map.from(_), mutable.HashMap.from(_))

object CausalID {
  final val ZERO: Integer = 0

  given partialOrder: PartialOrdering[CausalID] with {
    def lteq(x: CausalID, y: CausalID): Boolean = {
      val result = tryCompare(x, y)
      result match {
        case Some(x) if x < 0  => true
        case Some(x) if x == 0 => true
        case Some(x) if x > 0  => false
        case None              => false
        case Some(_)           => throw IllegalStateException()
      }
    }

    override def tryCompare(left: CausalID, right: CausalID): Option[Int] = {
      var leftLarger = false
      var rightLarger = false
      left.foreachEntry((rid, counter) => {
        val leftValue = counter
        val rightValue = right.getOrElse(rid, ZERO)
        if (leftValue > rightValue) {
          leftLarger = true
        } else if (leftValue < rightValue) {
          rightLarger = true
        }
      })
      right.foreachEntry((rid, counter) => {
        val rightValue = counter
        val leftValue = left.getOrElse(rid, ZERO)
        if (leftValue > rightValue) {
          leftLarger = true
        } else if (leftValue < rightValue) {
          rightLarger = true
        }
      })
      if (leftLarger && rightLarger) {
        None
      } else if (leftLarger && !rightLarger) {
        Some(1)
      } else if (!leftLarger && rightLarger) {
        Some(-1)
      } else {
        Some(0)
      }
    }
  }
}
