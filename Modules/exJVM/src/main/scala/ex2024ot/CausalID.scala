package ex2024ot

import text_rdt.{CollaborativeTextEditingAlgorithm, RID}

import scala.annotation.tailrec
import scala.collection.mutable

type CausalID = mutable.HashMap[RID, Int]

object CausalID {
  assert(!CausalID.partialOrder.lt(mutable.HashMap("A" -> 1, "B" -> 1), mutable.HashMap("B" -> 2)))

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
      var leftLarger  = false
      var rightLarger = false
      left.foreachEntry { (rid, counter) =>
        val leftValue  = counter
        val rightValue = right.getOrElse(rid, 0)
        if leftValue > rightValue then {
          leftLarger = true
        } else if leftValue < rightValue then {
          rightLarger = true
        }
      }
      right.foreachEntry { (rid, counter) =>
        val rightValue = counter
        val leftValue  = left.getOrElse(rid, 0)
        if leftValue > rightValue then {
          leftLarger = true
        } else if leftValue < rightValue then {
          rightLarger = true
        }
      }
      if leftLarger && rightLarger then {
        None
      } else if leftLarger && !rightLarger then {
        Some(1)
      } else if !leftLarger && rightLarger then {
        Some(-1)
      } else {
        Some(0)
      }
    }
  }

  given totalOrder: Ordering[CausalID] with {
    def compare(x: CausalID, y: CausalID): Int =
      partialOrder.tryCompare(x, y) match {
        case None =>
          @tailrec
          def smaller(remaining: List[RID]): Int = remaining match {
            case key :: t =>
              val result = x.getOrElse(key, 0).compare(y.getOrElse(key, 0))
              if result == 0 then smaller(t) else result
            case Nil =>
              0
          }
          val keys = (x.keysIterator ++ y.keysIterator).toList.sorted
          smaller(keys)
        case Some(value) => value
      }
  }
}
