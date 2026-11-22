package channels.connection

import java.nio.ByteBuffer

class ReceiveBufferTest extends munit.FunSuite {

  /** Fills `buffer` with `data`, as a channel providing at most what fits would. */
  def append(buffer: ReceiveBuffer, data: Array[Byte]): Unit = {
    var offset = 0
    while offset < data.length do
        offset += buffer.fill { bb =>
          val n = math.min(bb.remaining(), data.length - offset)
          bb.put(data, offset, n)
          n
        }
  }

  test("fifo across growing and compacting") {
    val buffer = ReceiveBuffer(4)
    val data   = Array.tabulate[Byte](1000)(_.toByte)
    append(buffer, data.take(300))
    assertEquals(buffer.take(100).toSeq, data.slice(0, 100).toSeq)
    append(buffer, data.drop(300))
    assertEquals(buffer.available, 900)
    buffer.consume(50)
    buffer.reserve(2000)
    assertEquals(buffer.take(850).toSeq, data.drop(150).toSeq)
    assertEquals(buffer.available, 0)
  }

  test("peekInt and view do not consume") {
    val buffer = ReceiveBuffer(8)
    append(buffer, ByteBuffer.allocate(8).putInt(-2).putInt(42).array())
    assertEquals(buffer.peekInt(), -2)
    buffer.consume(4)
    val view = buffer.view
    assertEquals(view.position(), 0)
    assertEquals(view.getInt(), 42)
    assertEquals(buffer.available, 4)
    buffer.consume(view.position())
    assertEquals(buffer.available, 0)
  }

  /** The free space `fill` offers, without adding anything. */
  def freeSpace(buffer: ReceiveBuffer): Int = {
    var free = 0
    buffer.fill { bb =>
      free = bb.remaining(); 0
    }: Unit
    free
  }

  test("reserve on a full buffer makes room for the rest") {
    val buffer = ReceiveBuffer(1024)
    append(buffer, new Array(1024))
    buffer.reserve(1024 + 512)
    assert(freeSpace(buffer) >= 512)
    assertEquals(buffer.available, 1024)
  }

  test("fill on a full buffer frees at least as much as is buffered") {
    for consumed <- List(0, 1, 511, 512, 1000, 1023) do {
      val buffer = ReceiveBuffer(1024)
      append(buffer, new Array(1024))
      buffer.consume(consumed)
      assert(freeSpace(buffer) >= math.max(1, 1024 - consumed), s"consumed $consumed")
      assertEquals(buffer.available, 1024 - consumed)
    }
  }

  test("fill passes on end of stream") {
    val buffer = ReceiveBuffer()
    assertEquals(buffer.fill(_ => -1), -1)
    assertEquals(buffer.available, 0)
  }
}
