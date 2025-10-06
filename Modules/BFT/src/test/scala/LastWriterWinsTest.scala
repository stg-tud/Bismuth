import OpBased.LastWriterWins

class LastWriterWinsTest extends munit.FunSuite:

  test("LWW test") {
    var r1 = LastWriterWins[String]()
    r1 = r1.merge(r1.write("hello"))

    var r2 = LastWriterWins[String]()
    r2 = r2.merge(r2.write("hi"))

    r1 = r1.merge(r2)
    r2 = r2.merge(r1)

    assertEquals(r1.read, r2.read)
  }
