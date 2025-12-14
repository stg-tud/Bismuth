package propertybasedtests

import datatypes.Counter
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class CounterPropertyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[Counter => Counter] =
    Gen.oneOf(
      (c: Counter) => c.inc,
      (c: Counter) => c.dec,
      (c: Counter) => c.add(Gen.choose(-10, 10).sample.getOrElse(1))
    )

  val genOps: Gen[List[Counter => Counter]] = Gen.listOf(genOp)

  def applyOps(c: Counter, ops: List[Counter => Counter]): Counter =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(Counter(), opsA)
      val b = applyOps(Counter(), opsB)
      assertEquals(a.merge(b).value, b.merge(a).value)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(Counter(), opsA)
      val b = applyOps(Counter(), opsB)
      val c = applyOps(Counter(), opsC)
      assertEquals(a.merge(b).merge(c).value, a.merge(b.merge(c)).value)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(Counter(), ops)
      assertEquals(a.merge(a).value, a.value)
    }
  }

  property("value matches sum of events") {
    forAll(genOps) { ops =>
      val c = applyOps(Counter(), ops)

      val eventSum =
        c.hashDAG.events.values
          .filter(e => !c.hashDAG.autohrIsByzantine(e.author) && e.id != "0")
          .map(_.content.get)
          .sum

      assertEquals(c.value, eventSum)
    }
  }
}
