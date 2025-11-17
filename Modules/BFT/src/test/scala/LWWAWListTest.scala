import datatypes.LWWAWList

class LWWAWListTest extends munit.FunSuite:
    test("basic") {
      var list = LWWAWList[String]()

      list = list.merge(list.add(0, "replica_1"))
      // list = list.merge(list.add(0, "hello"))

      val delta = list.remove(0)
      list = list.merge(delta)

      var list2 = LWWAWList[String]()
      list2 = list2.merge(list2.add(0, "replica_2"))
      // list2 = list2.merge(list2.add(0, "aaa"))

      list = list.merge(list2)
      list2 = list2.merge(list)

      println(list.list)
      println(list2.list)
    }

    test("basic 2") {
      var list = LWWAWList[String]()

      list = list.merge(list.add(0, "replica_1"))
      list = list.merge(list.add(0, "hello"))

      val delta = list.remove(1)
      list = list.merge(delta)

      var list2 = LWWAWList[String]()
      list2 = list2.merge(list2.add(0, "replica_2"))
      list2 = list2.merge(list2.remove(0))
      // list2 = list2.merge(list2.add(0, "aaa"))

      list = list.merge(list2)
      list2 = list2.merge(list)

      println(list.list)
      println(list2.list)
    }

    test("basic 3") {
      var list1 = LWWAWList[String]()
      var list2 = LWWAWList[String]()

      list1 = list1.merge(list1.add(0, "a"))
      list1 = list1.merge(list1.add(1, "b"))
      list1 = list1.merge(list1.add(2, "c"))

      list1 = list1.merge(list2)
      list2 = list2.merge(list1)

      list1 = list1.merge(list1.add(0, "x"))
      list2 = list2.merge(list2.remove(2))
      println(list2.list)

      list1 = list1.merge(list2)
      list2 = list2.merge(list1)

      println(list1.list)
      println(list2.list)
    }
