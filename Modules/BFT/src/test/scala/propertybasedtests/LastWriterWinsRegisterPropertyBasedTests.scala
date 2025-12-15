package propertybasedtests

import datatypes.LastWriterWinsRegister
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LastWriterWinsRegisterPropertyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[LastWriterWinsRegister[String] => LastWriterWinsRegister[String]] =
    (c: LastWriterWinsRegister[String]) => c.write(Gen.alphaNumStr.sample.get)

  val genOps: Gen[List[LastWriterWinsRegister[String] => LastWriterWinsRegister[String]]] = Gen.listOf(genOp)

  def applyOps(
                c: LastWriterWinsRegister[String],
                ops: List[LastWriterWinsRegister[String] => LastWriterWinsRegister[String]]
  ): LastWriterWinsRegister[String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(LastWriterWinsRegister[String](), opsA)
      val b = applyOps(LastWriterWinsRegister[String](), opsB)

      assertEquals(a.merge(b).read, b.merge(a).read)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(LastWriterWinsRegister[String](), opsA)
      val b = applyOps(LastWriterWinsRegister[String](), opsB)
      val c = applyOps(LastWriterWinsRegister[String](), opsC)
      assertEquals(a.merge(b).merge(c).read, a.merge(b.merge(c)).read)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LastWriterWinsRegister[String](), ops)
      assertEquals(a.merge(a).read, a.read)
    }
  }
}
