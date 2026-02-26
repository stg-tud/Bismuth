package lofi_acl.sync

import lofi_acl.sync.SynchronizedMutableArrayDeltaStore.{calculateResize, resize}
import rdts.time.{Dot, Dots}

import scala.reflect.ClassTag

class SynchronizedMutableArrayDeltaStore[RDT: ClassTag] {
  private var arrays: Map[String, Array[RDT]] = Map.empty
  @volatile private var _dots: Dots           = Dots.empty

  def dots: Dots = _dots

  def get(dot: Dot): Option[RDT] = synchronized {
    if !dots.contains(dot) then None
    else Some(arrays(dot.place.delegate)(dot.time.toInt))
  }

  def getAll(dots: Dots): Array[(Dot, RDT)] = synchronized {
    // TODO: Could also copy range from buffer.
    require(dots.size <= Int.MaxValue)
    val retrievable  = this.dots.intersect(dots)
    val resultBuffer = Array.ofDim[(Dot, RDT)](retrievable.size.toInt)

    var resultBufferIdx = 0
    retrievable.internal.foreach { (uid, ranges) =>
      arrays.get(uid.delegate) match {
        case Some(bufferOfReplica) =>
          val times = ranges.iterator
          times.foreach(time =>
              resultBuffer(resultBufferIdx) = Dot(uid, time) -> bufferOfReplica(time.toInt)
              resultBufferIdx += 1
          )
        case None =>
      }
    }

    resultBuffer
  }

  /** Adds or replaces a delta. */
  def put(dot: Dot, delta: RDT): Unit = synchronized {
    require(dot.time >= 0 && dot.time <= Int.MaxValue)
    _dots = dots.add(dot)
    val idx = dot.time.toInt
    arrays.get(dot.place.delegate) match {
      case Some(array) =>
        val newLength = calculateResize(array.length, idx + 1)
        if newLength < 0 then array(idx) = delta
        else
            val resizedArray = resize(array, newLength)
            resizedArray(idx) = delta
            arrays = arrays.updated(dot.place.delegate, resizedArray)
      case None =>
        val buffer = new Array[RDT](math.max(16, idx + 1))
        buffer(idx) = delta
        this.arrays = arrays.updated(dot.place.delegate, buffer)
    }
  }

  // Lazy removal -> doesn't shrink or free array, only changes tracked dots
  def removed(removedDots: Dots): Unit = synchronized {
    _dots = this._dots.subtract(removedDots)
  }
}

object SynchronizedMutableArrayDeltaStore {
  private inline def calculateResize(currentSize: Int, requiredSize: Int): Int = {
    val max = 2147483639
    require(requiredSize <= max)
    if currentSize >= requiredSize then -1
    else math.max(currentSize * 2, max)
  }

  private inline def resize[T: ClassTag](array: Array[T], newSize: Int): Array[T] = {
    val largerArray = new Array[T](newSize)
    System.arraycopy(array, 0, largerArray, 0, array.length)
    largerArray
  }
}
