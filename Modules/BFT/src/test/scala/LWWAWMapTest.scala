import datatypes.LWWAWMap

class LWWAWMapTest extends munit.FunSuite:
    test("test") {
      var map1 = LWWAWMap[String, String]()
      var map2 = LWWAWMap[String, String]()

      map1 = map1.merge(map1.put("1", "abc"))
      map1 = map1.merge(map1.remove("1"))
      map2 = map2.merge(map2.put("1", "def"))

      // map1 = map1.merge(map2)
      map2 = map2.merge(map1)

      println(map1.map)
      println(map2.map)
    }
