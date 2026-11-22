package channels.connection

import java.nio.ByteBuffer

/** Bytes received on a stream but not yet consumed by a protocol parser.
  *
  * Protocols parse from the front ([[available]], [[peekInt]], [[view]], [[consume]], [[take]]), the transport
  * appends at the back ([[fill]]). Unlike a raw `ByteBuffer`, there is no read/write mode: parsers that find an
  * incomplete message just leave the bytes in place and try again once more arrived.
  *
  * Grows as needed: use [[reserve]] when the size of an incomplete message is known, to receive it without repeated
  * growing.
  */
final class ReceiveBuffer(initialCapacity: Int = 1024) {
  require(initialCapacity > 0, "capacity must be positive")
  private var bytes: Array[Byte] = new Array(initialCapacity)
  private var start: Int         = 0
  private var end: Int           = 0

  /** Number of unconsumed bytes. */
  def available: Int = end - start

  /** Appends bytes using `read`, which gets a buffer of free space in write mode and returns the number of bytes read
    * (or -1 at the end of stream, as NIO channels do). Makes room first if there is no free space.
    */
  def fill(read: ByteBuffer => Int): Int = {
    ensureFree(1)
    val n = read(ByteBuffer.wrap(bytes, end, bytes.length - end))
    if n > 0 then end += n
    n
  }

  /** Makes sure the buffer can hold `size` unconsumed bytes in total (e.g., a whole message, of which some may
    * already be here) without making room again.
    */
  def reserve(size: Int): Unit = ensureFree(size - available)

  /** Makes sure there are at least `free` bytes of space after the unconsumed ones.
    *
    * If not, moves the unconsumed bytes to the front of the current array, if they and `free` fit into half of it.
    * Otherwise moves them to a new array, at least double the size. Either way, afterwards at least as much space is
    * free as was copied, so copying costs amortized constant time per byte.
    */
  private def ensureFree(free: Int): Unit =
    if bytes.length - end < free then {
      val needed = available + free
      val target =
        if needed <= bytes.length / 2 then bytes
        else new Array[Byte](math.max(needed, bytes.length * 2))
      System.arraycopy(bytes, start, target, 0, available)
      end = available
      start = 0
      bytes = target
    }

  /** The big-endian int at the front, without consuming it. Requires `available >= 4`. */
  def peekInt(): Int = {
    require(available >= 4, "not enough bytes")
    ((bytes(start) & 0xff) << 24) | ((bytes(start + 1) & 0xff) << 16) |
    ((bytes(start + 2) & 0xff) << 8) | (bytes(start + 3) & 0xff)
  }

  /** A read-only buffer of the unconsumed bytes, starting at position 0. Reading from it consumes nothing: parsers
    * call [[consume]] with the resulting position once they are done.
    */
  def view: ByteBuffer = ByteBuffer.wrap(bytes, start, available).slice().asReadOnlyBuffer()

  /** Drops `n` bytes from the front. */
  def consume(n: Int): Unit = {
    require(n <= available, "not enough bytes")
    start += n
    if start == end then {
      start = 0
      end = 0
    }
  }

  /** Removes and returns `n` bytes from the front. */
  def take(n: Int): Array[Byte] = {
    require(n <= available, "not enough bytes")
    val res = java.util.Arrays.copyOfRange(bytes, start, start + n)
    consume(n)
    res
  }
}
