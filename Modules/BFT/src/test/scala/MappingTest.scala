import riblt.Mapping
import java.security.MessageDigest

class MappingTest extends munit.FunSuite:
  test("test mapping") {
    val m = Mapping("hello".getBytes)
    var i = 0
    while i < 100 do {
      i = i + 1
      print(s"$i: ")
      print(m.nextIndex)
      print("\n")
    }

  }
