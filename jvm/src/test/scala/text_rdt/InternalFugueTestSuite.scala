package text_rdt

import munit.FunSuite

class SimpleInternalFugueTestSuite
    extends InternalFugueTestSuite(() => {
      SimpleFugueFactory.simpleFugueFactory
    }) {}

class ComplexInternalFugueTestSuite
    extends InternalFugueTestSuite(() => {
      ComplexFugueFactory.complexFugueFactory
    }) {
  test("batching-works") {
    val replicaStateA =
      Replica(ReplicaState("A")(using
        ComplexFugueFactory.complexFugueFactory
      ), StringEditory())
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.state.factory.tree.size, 2)
  }
}

class SimpleAVLInternalFugueTestSuite
    extends InternalFugueTestSuite(() => {
      SimpleAVLFugueFactory.simpleAVLFugueFactory
    }) {}
class ComplexAVLInternalFugueTestSuite
    extends InternalFugueTestSuite(() => {
      ComplexAVLFugueFactory.complexAVLFugueFactory
    }) {

  test("batching-works") {
    val replicaStateA =
      Replica(ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      ), StringEditory())
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.state.factory.tree.size, 2)
  }

}

abstract class InternalFugueTestSuite(
    val factoryConstructor: () => FugueFactory
) extends FunSuite {

  test("regression-1") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, '/')
    assertEquals(replicaStateA.text(), "/")
    replicaStateA.insert(0, 'e')
    assertEquals(replicaStateA.text(), "e/")
    replicaStateA.insert(0, '{')
    assertEquals(replicaStateA.text(), "{e/")
  }

  test("regression-2") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, '9')
    replicaStateA.insert(1, 'F')

    replicaStateA.insert(1, 'e')
    replicaStateA.insert(0, '5')
    assertEquals(replicaStateA.text(), "59eF")
  }

  test("regression-3") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, 'n')
    replicaStateA.insert(1, 'T')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, '<')
    assertEquals(replicaStateA.text(), "n<3T")
  }

  test("regression-4") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, 'v')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, 'G')
    assertEquals(replicaStateA.text(), "vG3")
    replicaStateA.insert(3, 'B')
    assertEquals(replicaStateA.text(), "vG3B")
  }

  test("regression-5") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, 'v')
    assertEquals(replicaStateA.text(), "v")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "")
  }

  test("regression-6") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, 'M')
    replicaStateA.insert(1, 'T')
    assertEquals(replicaStateA.text(), "MT")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "M")
  }

  test("regression-7") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, 'h')
    replicaStateA.insert(1, 'p')
    assertEquals(replicaStateA.text(), "hp")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "p")
  }

  test("regression-8") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, ']')
    replicaStateA.insert(1, '8')
    replicaStateA.insert(2, 'o')
    assertEquals(replicaStateA.text(), "]8o")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "]o")
  }

  test("regression-9") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, '0')
    replicaStateA.insert(1, '1')
    replicaStateA.insert(2, '2')
    assertEquals(replicaStateA.text(), "012")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "12")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "2")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "")
  }

  test("regression-10") {
    val replicaStateA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'h')
    replicaStateA.insert(2, ',')
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "/,")
  }

  test("regression-11") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, '/')
    assertEquals(replicaA.text(), "/")
    assertEquals(replicaB.text(), "")
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "/")
    assertEquals(replicaB.text(), "/")
    replicaA.delete(0)
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "/")
    replicaB.delete(0)
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "")
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "")
  }

  test("regression-12") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, '$')
    replicaA.insert(1, 'c')
    replicaA.insert(1, 'B')
    replicaA.delete(0)
    replicaA.insert(1, 'e')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "Bec")
    assertEquals(replicaStateB.text(), "Bec")
  }

  test("regression-13") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaB.insert(0, 'O')
    replicaB.sync(replicaA)
    replicaB.insert(1, '4')
    replicaA.insert(1, '{')
    replicaB.sync(replicaA)
    assertEquals(
      replicaStateA.text(),
      replicaStateB.text()
    )
  }

  test("regression-14") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaB.insert(0, 'L')
    assertEquals(replicaStateB.text(), "L")
    replicaB.insert(1, 'L')
    assertEquals(replicaStateB.text(), "LL")
    replicaB.insert(2, '/')
    assertEquals(replicaStateB.text(), "LL/")
    replicaB.insert(3, 'A')
    assertEquals(replicaStateB.text(), "LL/A")
    replicaB.insert(2, 'y')
    assertEquals(replicaStateB.text(), "LLy/A")
    replicaB.delete(0)
    assertEquals(replicaStateB.text(), "Ly/A")
    replicaB.insert(4, 'A')
    assertEquals(replicaStateB.text(), "Ly/AA")
    replicaB.sync(replicaA)
  }

  test("regression-15") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "AB")
    assertEquals(replicaStateB.text(), "AB")
  }

  test("regression-16") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "BA")
    assertEquals(replicaStateB.text(), "BA")
  }

  test("regression-17") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.insert(1, 'b')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "ABb")
    assertEquals(replicaStateB.text(), "ABb")
  }

  test("regression-18") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.insert(1, 'b')
    replicaB.syncFrom(replicaA.state)
    assertEquals(replicaStateB.text(), "BbA")
  }

  test("regression-19") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaB =
      Replica(ReplicaState("B")(using factoryConstructor()), StringEditory())
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaA.syncFrom(replicaB.state)
  }

  test("regression-20") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'a')
    replicaB.syncFrom(replicaStateA)
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaA.syncFrom(replicaStateB)
  }

  test("regression-21") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'a')
    replicaA.insert(1, 'c')
    replicaA.insert(1, 'b')
    replicaB.sync(replicaA)
  }

  test("regression-22") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'a')
    replicaA.sync(replicaB)
    replicaB.delete(0)
    replicaA.insert(1, 'c')
    replicaB.syncFrom(replicaStateA)
    assertEquals(replicaB.text(), "c")
  }

  test("regression-23") {
    val replicaA =
      Replica(ReplicaState("A")(using factoryConstructor()), StringEditory())
    replicaA.insert(0, 'A')
    assertEquals(replicaA.text(), "A")
    replicaA.insert(1, 'B')
    assertEquals(replicaA.text(), "AB")
    replicaA.insert(0, 'C')
    assertEquals(replicaA.text(), "CAB")
    replicaA.insert(2, 'D')
    assertEquals(replicaA.text(), "CADB")
  }

  test("regression-24") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'a')
    replicaA.insert(1, 'b')
    replicaA.insert(2, 'c')
    replicaA.sync(replicaB)
    replicaB.insert(2, 'd')
    replicaA.delete(2)
    replicaA.delete(1)
    replicaA.delete(0)
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "d")
    assertEquals(replicaB.text(), "d")
  }

  test("regression-25") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'O')
    replicaA.insert(1, 'V')
    replicaA.insert(2, 'Z')
    replicaA.sync(replicaB)
    replicaB.insert(2, '>')
    replicaA.delete(0)
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "V>Z")
    assertEquals(replicaB.text(), "V>Z")
  }

  test("regression-26") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'm')
    replicaA.insert(1, ' ')
    replicaA.delete(1)
    replicaA.sync(replicaB)
  }

  test("regression-27") {
    val replicaStateA =
      ReplicaState("D")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateC =
      ReplicaState("C")(using factoryConstructor())
    val replicaC = Replica(replicaStateC, StringEditory())
    replicaA.insert(0, 'B')
    replicaA.insert(0, '@')
    replicaA.insert(2, 'u')
    replicaA.insert(1, ')')
    replicaA.insert(2, 'M')
    replicaA.sync(replicaC)
    replicaA.delete(0)
    replicaC.delete(1)
    replicaA.sync(replicaC)
    assertEquals(replicaA.text(), "MBu")
    assertEquals(replicaC.text(), "MBu")
  }

  test("regression-28") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'D')
    replicaA.insert(1, '+')
    replicaA.insert(2, 's')
    replicaA.delete(0)
    replicaA.delete(0)
    replicaA.sync(replicaB)
    assertEquals(replicaA.editor.asInstanceOf[StringEditory].data.toString(), "s")
    assertEquals(replicaB.editor.asInstanceOf[StringEditory].data.toString(), "s")
    assertEquals(replicaA.text(), replicaB.text())
    assertEquals(replicaA.text(), "s")
    assertEquals(replicaB.text(), "s")
  }

  test("2023-weidner-minimizing-interleaving-figure-1") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, StringEditory())
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "")
    replicaA.insert(0, 'm')
    replicaA.insert(1, 'i')
    replicaA.insert(2, 'l')
    replicaA.insert(3, 'k')
    replicaA.insert(4, '\n')
    assertEquals(replicaA.text(), "milk\n")
    assertEquals(replicaB.text(), "")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milk\n")
    assertEquals(replicaB.text(), "milk\n")
    replicaA.insert(4, '\n')
    replicaA.insert(5, 'e')
    replicaA.insert(6, 'g')
    replicaA.insert(7, 'g')
    replicaA.insert(8, 's')
    assertEquals(replicaA.text(), "milk\neggs\n")
    assertEquals(replicaB.text(), "milk\n")
    replicaB.insert(4, '\n')
    replicaB.insert(5, 'b')
    replicaB.insert(6, 'r')
    replicaB.insert(7, 'e')
    replicaB.insert(8, 'a')
    replicaB.insert(9, 'd')
    assertEquals(replicaA.text(), "milk\neggs\n")
    assertEquals(replicaB.text(), "milk\nbread\n")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milk\neggs\nbread\n")
    assertEquals(replicaB.text(), "milk\neggs\nbread\n")
  }

}
