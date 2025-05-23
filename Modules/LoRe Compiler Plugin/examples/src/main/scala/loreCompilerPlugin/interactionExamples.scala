package loreCompilerPlugin

import lore.dsl.*
import loreCompilerPlugin.annotation.LoReProgram
import reactives.default.{Signal as Derived, Var as Source}
import scala.collection.immutable.Map

@LoReProgram
object interactionExamples:
  val tupleThing: (Int, Int) = (1, 2)
  // ========= Misc tests =========
  val arrowFun: (Int, String) => Int = (x: Int, y: String) => x * 2

  val emptyList: List[Int]     = List()
  val stringList: List[String] = List("foo bar", "baz qux")

  // Map syntax "x -> y" in RHS currently does not work
  val emptyMap: Map[Int, String]            = Map()
  val stringMap: Map[Int, (String, String)] = Map((0, ("foo", "bar")), (1, Tuple2("baz", "qux")))

  // ========= Interaction tests =========

  // ** BEGIN SOURCES
  val integerLiteral: Int                 = 2
  val integerLiteralSource: Source[Int]   = Source(1)
  val integerReferenceSource: Source[Int] = Source(integerLiteral)

  // Binary integer operators
  val integerLiteralAdditionSource: Source[Int]       = Source(4 + 2)
  val integerLiteralSubtractionSource: Source[Int]    = Source(4 - 2)
  val integerLiteralMultiplicationSource: Source[Int] = Source(4 * 2)
  val integerLiteralDivisionSource: Source[Int]       = Source(4 / 2)

  // Binary integer operators with references
  val integerReferenceBinaryOpSource1: Source[Int] = Source(integerLiteral + integerLiteral)
  val integerReferenceBinaryOpSource2: Source[Int] = Source(4 - integerLiteral)
  val integerReferenceBinaryOpSource3: Source[Int] = Source(integerLiteral * 2)
  // ** END SOURCES

  val integerInteraction1 = Interaction[Int, Int]

  val integerInteraction2 = Interaction[Int, Int]
    .requires { (a: Int, b: Int) => a > b }

  val integerInteraction3 = Interaction[Int, Int]
    .requires { (a: Int, b: Int) => a > b }
    .executes { (a: Int, b: Int) => a }

  val integerInteraction4 = Interaction[Int, Int]
    .requires { (a: Int, b: Int) => a > b }
    .executes { (a: Int, b: Int) => a }
    .ensures { (a: Int, b: Int) => a > b }

  val integerSource: Source[Int]   = Source(1)
  val integerDerived: Derived[Int] = Derived { integerSource.value + integerSource.value }

  val integerInteraction5 = Interaction[Int, Int]
    .requires { (curr: Int, _) => curr > 1 }
    .modifies { integerSource }
    .executes { (curr: Int, _) => curr + 3 }
    .ensures { (curr: Int, _) => curr > 5 }

  val integerInteraction6 = Interaction[Int, Int]
    .requires { (a: Int, b: Int) => 5 + 2 == 1 }
    .modifies { integerReferenceBinaryOpSource1 }
    .executes { (a: Int, b: Int) =>
      {
        print("aaa")
        if 1 > 2 then print("bbb") else print("123")
        print("ccc")
        a + b
      }
    }
    .ensures { (a: Int, b: Int) => false }

  val integerInteraction7 = integerInteraction4.modifies { integerLiteralSource }

  val xy: Invariant   = Invariant(1 < 2)
  val inv2: Invariant = Invariant(integerReferenceBinaryOpSource1.value + 1 > integerReferenceBinaryOpSource1.value)
  val inv3: Invariant =
    Invariant(integerReferenceBinaryOpSource1.value + integerSource.value > integerReferenceBinaryOpSource1.value)
  val inv4: Invariant = Invariant(integerSource.value - 1 < integerSource.value)
  val inv5: Invariant = Invariant(integerDerived.value > 1)
  val inv6: Invariant = Invariant(integerLiteral > 1)

  print("aaa")
  print("bbb")
  println("ccc")

  if true then print("aaaa") else print("bbbb")
end interactionExamples
