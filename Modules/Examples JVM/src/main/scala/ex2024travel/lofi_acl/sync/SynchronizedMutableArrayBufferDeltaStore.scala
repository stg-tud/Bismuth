package ex2024travel.lofi_acl.sync

import rdts.time.{Dot, Dots}

import scala.collection.mutable.ArrayBuffer

class SynchronizedMutableArrayBufferDeltaStore[RDT] {
  private var arrays: Map[String, ArrayBuffer[RDT]] = Map.empty
  @volatile private var _dots: Dots                 = Dots.empty

  def dots: Dots = _dots

  def get(dot: Dot): Option[RDT] = synchronized {
    arrays.get(dot.place.delegate) match {
      case Some(buffer) =>
        require(dot.time >= 0 && dot.time <= Int.MaxValue)
        if buffer.length <= dot.time then None
        else Option(buffer(dot.time.toInt))
      case None => None
    }
  }

  def getAll(dots: Dots): Array[(Dot, RDT)] = synchronized {
    require(dots.size <= Int.MaxValue)
    val resultBuffer = Array.ofDim[(Dot, RDT)](dots.size.toInt)

    var resultBufferIdx = 0
    dots.internal.foreach { (uid, ranges) =>
      arrays.get(uid.delegate) match {
        case Some(bufferOfReplica) =>
          val times = ranges.iterator
          times.takeWhile(_ < bufferOfReplica.length).foreach(time =>
              resultBuffer(resultBufferIdx) = Dot(uid, time) -> bufferOfReplica(time.toInt)
              resultBufferIdx += 1
          )
        case None =>
      }
    }

    resultBuffer.slice(0, resultBufferIdx)
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
        val buffer = ArrayBuffer.empty(dot.time.toInt)
        this.arrays = arrays.updated(dot.place.delegate, buffer)
        put(dot, delta)
    }
  }
}
