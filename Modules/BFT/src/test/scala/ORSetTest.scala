import datatypes.ORSet

class ORSetTest extends munit.FunSuite:
  test("add element to empty set") {
    var set = ORSet[String]()
    set     = set.merge(set.add("x"))

    assertEquals(set.getElements, Set("x"))
  }

  test("remove element from empty set") {
    var set = ORSet[String]()
    set     = set.merge(set.remove("x"))

    assertEquals(set.getElements, Set.empty)
  }

  test("remove existing element") {
    var set = ORSet[String]()
    set     = set.merge(set.add("x"))
    set     = set.merge(set.remove("x"))

    assertEquals(set.getElements, Set.empty)
  }

  test("synchronise 2 sets: example 1") {
    var set1 = ORSet[String]()
    set1     = set1.merge(set1.add("x"))
    set1     = set1.merge(set1.remove("y"))

    var set2 = ORSet[String]()
    set2     = set2.merge(set2.add("y"))
    set2     = set2.merge(set2.remove("x"))

    set1 = set1.merge(set2)
    set2 = set2.merge(set1)

    assertEquals(set1.getElements, Set("x", "y"))
    assertEquals(set1.getElements, set2.getElements)
  }

  test("synchronise 2 sets: example 2") {
    var set1 = ORSet[String]()
    set1     = set1.merge(set1.add("x"))
    set1     = set1.merge(set1.remove("y"))

    var set2 = ORSet[String]()
    set2     = set2.merge(set2.add("y"))
    set2     = set2.merge(set2.remove("x"))

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

  test("synchronise with riblt") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("a"))
    set1 = set1.merge(set1.remove("b"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("a"))
    set2 = set2.merge(set2.remove("c"))
    
    val c = set1.produceNextCodedSymbols()
    set2 = set2.addCodedSymbols(c)
    while !set2.riblt.isDecoded do
      set2 = set2.addCodedSymbols(set1.produceNextCodedSymbols())


    val synReq = set2.sendSyncRequest
    val response = set1.receiveSyncRequest(synReq)
    set1 = response._1
    set2 = set2.merge(response._2)

    assertEquals(set1.getElements, set2.getElements)
  }

