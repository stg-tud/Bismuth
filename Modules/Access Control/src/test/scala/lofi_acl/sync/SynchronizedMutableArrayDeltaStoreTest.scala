package lofi_acl.sync

import munit.FunSuite
import rdts.base.Uid
import rdts.time.Dot

class SynchronizedMutableArrayDeltaStoreTest extends FunSuite {

  val A: Uid = Uid("A")
  val B: Uid = Uid("B")

  def newStore: SynchronizedMutableArrayDeltaStore[String] =
    new SynchronizedMutableArrayDeltaStore[String]

  test("add after end of array") {
    val uut = newStore
    uut.put(Dot(A, 42), "123")
  }

  test("add multiple") {
    val uut = newStore
    uut.put(Dot(A, 0), "123")
    uut.put(Dot(B, 1), "1234")
    uut.put(Dot(B, 15), "1234")
    uut.put(Dot(B, 16), "1234")
    uut.put(Dot(B, 129), "1234")
  }

}
