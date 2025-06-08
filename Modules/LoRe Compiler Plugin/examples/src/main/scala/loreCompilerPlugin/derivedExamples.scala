package loreCompilerPlugin

import lore.dsl.*
import loreCompilerPlugin.annotation.LoReProgram
import reactives.default.{Signal as Derived, Var as Source}

def foo(bar: Int, baz: String): Int = 0

@LoReProgram
object derivedExamples:
  // ========= Plain values and sources to use in below tests =========
  val integerLiteral: Int     = 1
  val stringLiteral: String   = "foo"
  val booleanLiteral: Boolean = true

  // The following 4 examples are supported in the frontend but not the backend
//    val functionCall: Int            = foo(1, "foobar")
//    val methodCallNoParens: Int      = 1.toInt
//    val methodCallWithParens: String = "foo".strip()
//    val methodCallWithParams: String = "foo".repeat(5)

  val integerLiteralSource1: Source[Int]  = Source(1)
  val integerLiteralSource2: Source[Int]  = Source(2)
  val stringLiteralSource: Source[String] = Source("bar")
  val boolLiteralSource: Source[Boolean]  = Source(false)

  // ========= Derived tests =========

  // These will have constant values, as they depend on values that are not mutable
  val integerLiteralDerived: Derived[Int]    = Derived { 1 + 2 }
  val integerReferenceDerived1: Derived[Int] = Derived { integerLiteral + 2 }
  val integerReferenceDerived2: Derived[Int] = Derived { 1 + integerLiteral }

  // Single and double dependencies on integer sources
  val integerSourceDerived1: Derived[Int] = Derived { integerLiteralSource1.value + 2 }
  val integerSourceDerived2: Derived[Int] = Derived { 1 + integerLiteralSource2.value }
  val integerSourceDerived3: Derived[Int] = Derived { integerLiteralSource1.value + integerLiteralSource2.value }
end derivedExamples
