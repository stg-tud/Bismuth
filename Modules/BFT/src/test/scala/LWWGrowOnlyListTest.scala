import datatypes.LWWGrowOnlyList

class LWWGrowOnlyListTest extends munit.FunSuite {
  test("basic") {
    var list1 = LWWGrowOnlyList[String]()
    list1 = list1.merge(list1.add(0, "a"))
    println(list1.list)

    list1 = list1.merge(list1.add(0, "b"))
    println(list1.list)

    list1 = list1.merge(list1.add(1, "c"))
    println(list1.list)

    list1 = list1.merge(list1.add(2, "d"))
    println(list1.list)

    list1 = list1.merge(list1.add(4, "e"))

    println(list1.list)

    var list2 = LWWGrowOnlyList[String]()
    list2 = list2.merge(list2.add(0, "hh"))
    list2 = list2.merge(list2.add(0, "oo"))
    list2 = list2.merge(list2.add(1, "xx"))
    list2 = list2.merge(list2.add(2, "pmpmp"))
    list2 = list2.merge(list2.add(4, "wwww"))

    println(list1.list)
    println(list2.list)

    list1 = list1.merge(list2)
    println(list1.list)

    list2 = list2.merge(list1)
    println(list2.list)

  }

}
