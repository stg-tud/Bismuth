import datatypes.ORSet

class ORSetTest extends munit.FunSuite:

  test("synchronise 2 sets") {
    val setA1 = ORSet[String]()
    val (setA2, event1) = setA1.add("x")
    val (setA3, event2) = setA2.remove("y")

    val setB1 = ORSet[String]()
    val (setB2, event3) = setB1.add("y")
    val (setB3, event4) = setB2.remove("x")

    val setA4 = setA3.receiveEvent(event3)
    val setA5 = setA4.receiveEvent(event4)

    val setB4 = setB3.receiveEvent(event1)
    val setB5 = setB4.receiveEvent(event2)

    assertEquals(setA5.elements, setB5.elements)
  }
