import datatypes.MultiValueRegister

class MultiValueRegisterTest extends munit.FunSuite:

  test("MVR: overriding previous value") {
    var r1 = MultiValueRegister[String]()
    r1 = r1.merge(r1.write("hello"))

    assertEquals(r1.read, Set(Option("hello")))

    r1 = r1.merge(r1.write("hi"))

    assertEquals(r1.read, Set(Option("hi")))

  }

  test("MVR: sync 2 replicas") {
    var r1 = MultiValueRegister[String]()
    var r2 = MultiValueRegister[String]()

    r1 = r1.merge(r1.write("hello"))
    r2 = r2.merge(r2.write("hi"))

    r1 = r1.merge(r2)
    r2 = r2.merge(r1)

    assertEquals(r1.read, r2.read)

  }
