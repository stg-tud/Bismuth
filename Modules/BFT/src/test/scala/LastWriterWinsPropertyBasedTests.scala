import datatypes.LastWriterWins
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LastWriterWinsPropertyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[LastWriterWins[String] => LastWriterWins[String]] =
    (c: LastWriterWins[String]) => c.write(Gen.alphaNumStr.sample.get)

  val genOps: Gen[List[LastWriterWins[String] => LastWriterWins[String]]] = Gen.listOf(genOp)

  def applyOps(
      c: LastWriterWins[String],
      ops: List[LastWriterWins[String] => LastWriterWins[String]]
  ): LastWriterWins[String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(LastWriterWins[String](), opsA)
      val b = applyOps(LastWriterWins[String](), opsB)

      assertEquals(a.merge(b).read, b.merge(a).read)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(LastWriterWins[String](), opsA)
      val b = applyOps(LastWriterWins[String](), opsB)
      val c = applyOps(LastWriterWins[String](), opsC)
      assertEquals(a.merge(b).merge(c).read, a.merge(b.merge(c)).read)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LastWriterWins[String](), ops)
      assertEquals(a.merge(a).read, a.read)
    }
  }
}
