import OpBased.ORSet

class ORSetTest extends munit.FunSuite:
  test("add element to empty set") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")

    assertEquals(setA2.getElements, Set("x"))
  }

  test("remove element from empty set") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.remove("x")

    assertEquals(setA2.getElements, Set.empty)
  }

  test("remove existing element") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")

    val (setA3, event2) = setA1.remove("x")

    assertEquals(setA3.getElements, Set.empty)
  }

  test("synchronise 2 sets: example 1") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")
    val (setA3, event2) = setA2.remove("y")

    val setB1           = ORSet[String]()
    val (setB2, event3) = setB1.add("y")
    val (setB3, event4) = setB2.remove("x")

    val setA4 = setA3.receiveEvent(event3)
    val setA5 = setA4.receiveEvent(event4)

    val setB4 = setB3.receiveEvent(event1)
    val setB5 = setB4.receiveEvent(event2)

    assertEquals(setA5.getElements, Set("x", "y"))
    assertEquals(setA5.getElements, setB5.getElements)
  }

  test("synchronise 2 sets: example 2") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")
    val (setA3, event2) = setA2.remove("y")

    val setB1           = ORSet[String]()
    val (setB2, event3) = setB1.add("y")
    val (setB3, event4) = setB2.remove("x")

    val setA4 = setA3.receiveEvent(event3)
    val setA5 = setA4.receiveEvent(event4)

    val setB4 = setB3.receiveEvent(event1)
    val setB5 = setB4.receiveEvent(event2)

    val (setA6, event5) = setA5.remove("y")
    val (setB6, event6) = setB5.remove("x")

    val setA7 = setA6.receiveEvent(event6)
    val setB7 = setB6.receiveEvent(event5)

    assertEquals(setA7.getElements, Set.empty)
    assertEquals(setA7.getElements, setB7.getElements)
  }

  test("synchronise 2 sets: example 3") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")
    val (setA3, event2) = setA2.remove("y")

    val setB1           = ORSet[String]()
    val (setB2, event3) = setB1.add("y")
    val (setB3, event4) = setB2.remove("x")

    val setA4 = setA3.receiveEvent(event3)
    val setA5 = setA4.receiveEvent(event4)

    val setB4 = setB3.receiveEvent(event1)
    val setB5 = setB4.receiveEvent(event2)

    val (setA6, event5) = setA5.remove("y")
    val (setB6, event6) = setB5.remove("x")

    val setA7 = setA6.receiveEvent(event6)
    val setB7 = setB6.receiveEvent(event5)

    val (setA8, event7) = setA7.add("y")
    val (setB8, event8) = setB7.add("x")

    val setA9 = setA8.receiveEvent(event8)
    val setB9 = setB8.receiveEvent(event7)

    assertEquals(setA9.getElements, Set("x", "y"))
    assertEquals(setA9.getElements, setB5.getElements)

  }

  test("synchronise 2 sets: example 4") {
    val setA1           = ORSet[String]()
    val (setA2, event1) = setA1.add("x")
    val (setA3, event2) = setA2.remove("x")

    val setB1           = ORSet[String]()
    val (setB2, event3) = setB1.add("x")

    val setB3 = setB2.receiveEvent(event1)
    val setB4 = setB3.receiveEvent(event2)

    val setA4 = setA3.receiveEvent(event3)

    assertEquals(setA4.getElements, Set("x"))
    assertEquals(setA4.getElements, setB4.getElements)
  }

  test("synchronise with riblt") {
    val setA1 = ORSet[String]()
    val (setA2, event1) = setA1.add("a")
    val (setA3, event2) = setA2.add("b")

    val setB1 = ORSet[String]()
    val (setB2, event3) = setB1.add("a")
    val (setB3, event4) = setB2.add("c")

    //val setB4 = setB3.decRestart()
    val c = setA3.produceNextCodedSymbols()
    var (setB4, isDecoded) = setB3.addCodedSymbols(c)
    while !isDecoded do
      val res = setB4.addCodedSymbols(setA3.produceNextCodedSymbols())
      setB4 = res._1
      isDecoded = res._2

    val diff = setB4.sendDiff
    val (setA4, response) = setA3.receiveDiff(diff._1, diff._2)
    val setB5 = setB4.receiveEvents(response)

    val setB6 = setB5.processQueue
    val setA5 = setA4.processQueue



    assertEquals(setA5.getElements, setB6.getElements)
  }

