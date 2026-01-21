package datatypes

import datatypes.LastWriterWinsRegister

class LastWriterWinsRegisterTest extends munit.FunSuite:

    test("LWW test") {
      var r1 = LastWriterWinsRegister[String]()
      r1 = r1.merge(r1.write("hello"))

      var r2 = LastWriterWinsRegister[String]()
      r2 = r2.merge(r2.write("hi"))

      r1 = r1.merge(r2)
      r2 = r2.merge(r1)

      assertEquals(r1.read, r2.read)
    }
