package com.softwaremill.quicklens
package test


object ExtensionCopyTest {
  case class V(x: Double, y: Double, z: Double)

  opaque type Vec = V

  object Vec {
    def apply(x: Double, y: Double): Vec = V(x, y, 0)

    extension (v: Vec) {
      def x: Double                                   = v.x
      def y: Double                                   = v.y
      def copy(x: Double = v.x, y: Double = v.y): Vec = V(x, y, 0)
    }
  }
}

class ExtensionCopyTest extends munit.FunSuite {
  test("modify a simple class with an extension copy method") {
    // this test does compile at the moment, because we search extensions in companions only
    /*
    class VecSimple(xp: Double, yp: Double) {
      val xMember = xp
      val yMember = yp
    }

    object VecSimple {
      def apply(x: Double, y: Double): VecSimple = new VecSimple(x, y)
    }

    extension (v: VecSimple) {
      def copy(x: Double = v.xMember, y: Double = v.yMember): VecSimple = new VecSimple(x, y)
    }
    val a = VecSimple(1, 2)
    val b = a.modify(_.xMember).using(_ + 10)
    b.xMember shouldEqual 11
     */
  }

  test("modify a simple class with an extension copy method in companion") {
    class VecCompanion(xp: Double, yp: Double) {
      val x = xp
      val y = yp
    }

    object VecCompanion {
      def apply(x: Double, y: Double): VecCompanion = new VecCompanion(x, y)
      extension (v: VecCompanion) {
        def copy(x: Double = v.x, y: Double = v.y): VecCompanion = new VecCompanion(x, y)
      }
    }

    val a = VecCompanion(1, 2)
    val b = a.modify(_.x).using(_ + 10)
    assert(b.x == 11.0)
  }

  test("modify a class with extension methods in companion") {
    case class V(xm: Double, ym: Double)

    class VecClass(val v: V)

    object VecClass {
      def apply(x: Double, y: Double): VecClass = new VecClass(V(x, y))

      extension (v: VecClass) {
        def x: Double                                        = v.v.xm
        def y: Double                                        = v.v.ym
        def copy(x: Double = v.x, y: Double = v.y): VecClass = new VecClass(V(x, y))
      }
    }

    val a = VecClass(1, 2)
    val b = a.modify(_.x).using(_ + 10)
    assert(b.x == 11.0)
  }

  test("modify an opaque type with extension methods") {
    import ExtensionCopyTest.*

    val a = Vec(1, 2)
    val b = a.modify(_.x).using(_ + 10)
    assert(b.x == 11.0)
  }
}
