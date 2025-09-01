import riblt.Mapping
import java.security.MessageDigest

class MappingTest extends munit.FunSuite:
  test("test mapping") {
    val e = MessageDigest.getInstance("SHA3-512").digest("a".getBytes)
    val m = Mapping(e)
    
  }
