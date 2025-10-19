import datatypes.ORSet

class ORSetTest extends munit.FunSuite:
  test("add element to empty set") {
    var set = ORSet[String]()
    set = set.merge(set.add("x"))

    assertEquals(set.getElements, Set("x"))
  }

  test("remove element from empty set") {
    var set = ORSet[String]()
    set = set.merge(set.remove("x"))

    assertEquals(set.getElements, Set.empty)
  }

  test("remove existing element") {
    var set = ORSet[String]()
    set = set.merge(set.add("x"))
    set = set.merge(set.remove("x"))

    assertEquals(set.getElements, Set.empty)
  }

  test("synchronise 2 sets: example 1") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("x"))
    set1 = set1.merge(set1.remove("y"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("y"))
    set2 = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    assertEquals(set1.getElements, Set("x", "y"))
    assertEquals(set1.getElements, set2.getElements)
  }

  test("synchronise 2 sets: example 2") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("x"))
    set1 = set1.merge(set1.remove("y"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("y"))
    set2 = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    set1 = set1.merge(set1.remove("y"))
    set2 = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    assertEquals(set1.getElements, Set.empty)
    assertEquals(set1.getElements, set2.getElements)
  }

  test("synchronise 2 sets: example 3") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("x"))
    set1 = set1.merge(set1.remove("y"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("y"))
    set2 = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    set1 = set1.merge(set1.remove("y"))
    set2 = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    set1 = set1.merge(set1.add("y"))
    set2 = set2.merge(set2.add("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    assertEquals(set1.getElements, Set("x", "y"))
    assertEquals(set1.getElements, set2.getElements)

  }

  test("synchronise 2 sets: example 4") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("x"))
    set1 = set1.merge(set1.remove("x"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    assertEquals(set1.getElements, Set("x"))
    assertEquals(set1.getElements, set2.getElements)
  }

  test("synchronise 2 sets with riblt") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("a"))
    set1 = set1.merge(set1.remove("b"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("a"))
    set2 = set2.merge(set2.remove("c"))

    while !set2.riblt.isDecoded do
      set2 = set2.addCodedSymbols(set1.produceNextCodedSymbols())

    val synReq = set2.sendSyncRequest
    set1 = set1.merge(synReq.delta)
    set2 = set2.merge(set1.sendSyncResponse(synReq.requestedEvents))

    assertEquals(set1.getElements, set2.getElements)
  }

  /*test("synchronise 3 sets with riblt") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("a"))
    set1 = set1.merge(set1.remove("b"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("a"))
    set2 = set2.merge(set2.remove("c"))

    var set3 = ORSet[String]()
    set3 = set3.merge(set3.add("d"))
    set3 = set3.merge(set3.remove("e"))

    while !set2.riblt.isDecoded || !set3.riblt.isDecoded do {
      val codedSymbol = set1.produceNextCodedSymbols()
      if !set2.riblt.isDecoded then {
        set2 = set2.addCodedSymbols(codedSymbol)
      }
      if !set3.riblt.isDecoded then
        set3 = set3.addCodedSymbols(codedSymbol)
    }

    val synReq1 = set2.sendSyncRequest
    set1 = set1.merge(synReq1.delta)
    set2 = set2.merge(set1.sendSyncResponse(synReq1.requestedEvents))

    val synReq2 = set3.sendSyncRequest
    set1 = set1.merge(synReq2.delta)
    set3 = set3.merge(set1.sendSyncResponse(synReq2.requestedEvents))

    assertEquals(set1.getElements, set2.getElements ++ set3.getElements)
  }*/
