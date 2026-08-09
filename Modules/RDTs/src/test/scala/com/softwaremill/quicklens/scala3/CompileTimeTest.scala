package com.softwaremill.deltalens.scala3

class CompileTimeTest extends munit.FunSuite {
  // #114
  test("not compile for too long in case of chained modify invocations") {
    val start = System.currentTimeMillis()
    assert(compileErrors("""
      case class B(a1: Double, a2: Double, a3: Double, a4: Double, a5: Double)
      case class C(b: B)
      
      import com.softwaremill.quicklens.*
      
      val c = C(B(1, 1, 1, 1, 1))
      c
        .modify(_.b.a1).setTo("")
        .modify(_.b.a2).setTo("")
        .modify(_.b.a3).setTo("")
        .modify(_.b.a4).setTo("")
        .modify(_.b.a5).setTo("")
  """).nonEmpty)
    val end = System.currentTimeMillis()
    assert((end - start) <= 5000L) // that's a lot anyway
  }
}
