package test.rdts.bespoke

import rdts.base.Uid
import rdts.datatypes.MultiVersionRegister
import test.rdts.given

class MultiVersionRegisterTest extends munit.FunSuite {

  test("basic usage") {

    val a = MultiVersionRegister.empty[String]

    val alice   = Uid.predefined("alice")
    val bob     = Uid.predefined("bob")
    val charlie = Uid.predefined("charlie")

    val b = a.write("hi")(using alice.convert)
    val c = a.write("ho")(using bob.convert)

    val m1 = b `merge` c

    assertEquals(m1.read, Set("hi", "ho"))

    val d = m1.write("lets go!")(using charlie.convert)

    assertEquals(d.read, Set("lets go!"))

    assertEquals(m1 `merge` d, d `merge` b)

  }

}
