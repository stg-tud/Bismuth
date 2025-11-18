package ex2024travel.lofi_acl.sync

import rdts.time.{Dot, Dots}

import scala.collection.mutable.ArrayBuffer

class SynchronizedMutableArrayBufferDeltaStore[RDT] {
  private var arrays: Map[String, ArrayBuffer[RDT]] = Map.empty
  @volatile private var _dots: Dots                 = Dots.empty

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
    arrays.get(dot.place.delegate) match {
      case Some(buffer) =>
        buffer.sizeHint(dot.time.toInt)
        buffer.insert(dot.time.toInt, delta)
        _dots = dots.add(dot)
      case None =>
        val buffer = ArrayBuffer.empty[RDT]
        this.arrays = arrays.updated(dot.place.delegate, buffer)
        put(dot, delta)
    }
  }

  // Lazy removal -> doesn't shrink or free array, only changes tracked dots
  def removed(removedDots: Dots): Unit = synchronized {
    _dots = this._dots.subtract(removedDots)
  }
}
