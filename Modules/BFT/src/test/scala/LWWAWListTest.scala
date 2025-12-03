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

      // println(list.list)
      // println(list2.list)
    }

    test("basic 2") {
      var list = LWWAWList[String]()

      list = list.merge(list.add(0, "replica_1"))
      list = list.merge(list.add(0, "hello"))

      val delta = list.remove(0)
      list = list.merge(delta)

      var list2 = LWWAWList[String]()
      list2 = list2.merge(list2.add(0, "replica_2"))
      list2 = list2.merge(list2.remove(0))
      // list2 = list2.merge(list2.add(0, "aaa"))

      list = list.merge(list2)
      list2 = list2.merge(list)

      // println(list.list)
      // println(list2.list)
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
      // println(list2.list)

      list1 = list1.merge(list2)
      list2 = list2.merge(list1)

      // println(list1.list)
      // println(list2.list)
    }

    test("basic 5") {
      var list1 = LWWAWList[String]()
      var list2 = LWWAWList[String]()

      list1 = list1.merge(list1.add(0, "a"))
      list1 = list1.merge(list1.add(1, "b"))
      list1 = list1.merge(list1.add(2, "c"))

      list1 = list1.merge(list2)
      list2 = list2.merge(list1)

      list1 = list1.merge(list1.add(2, "y"))

      list1 = list1.merge(list2)
      list2 = list2.merge(list1)

      // println(list1.list)
      // println(list2.list)
    }

    test("basic 4") {
      var list1 = LWWAWList[String]()
      var list2 = LWWAWList[String]()
      var list3 = LWWAWList[String]()

      // list1 = list1.merge(list1.add(0, "uzZUJ0sbe4JRNFFSHPduwVqA5nPS7JMULAdUmMik"))
      // list1 = list1.merge(list1.add(0, "fPwoGm8dztk7MLPE6od7EYM0l3GXHez8bA0opVWuG2yeYIsoc6gGVeCsOwBAOLBsHnwiTCnOnAgW6q9eYHuJAJ"))
      // list1 = list1.merge(list1.add(0, "1l58gSF2"))

      // list1 = list1.merge(list1.remove(0))

      list2 =
        list2.merge(list2.add(0, "hOwMyjPXPXc5XSA0LepkcJN3p90LsabnAODuLVvcYKahvBzUdO3QoQCl1L8mB42ZZKRZ3Zt5z569yZ3c88"))
      // list2 = list2.merge(list2.remove(0))
      list2 = list2.merge(list2.add(0, "5J"))

      list3 = list3.merge(list3.add(0, "4YVWMWQhtTQAsytxp008xDy"))
      list3 = list3.merge(list3.add(1, "3EWR18xckfkxoWHzFbEgvIiMS9kq9czU"))
      // list3 = list3.merge(list3.add(2, "k2HbTyTY5wyP2nHZGpjg"))
      list3 = list3.merge(list3.remove(1))
      // list3 = list3.merge(list3.remove(0))

      // println(list1.merge(list2).merge(list3).list)
      // println(list1.merge(list2.merge(list3)).list)

      val t1 = list1.merge(list2)
      val t2 = t1.merge(list3)

      val t3 = list2.merge(list3)
      val t4 = list1.merge(t3)

    }
