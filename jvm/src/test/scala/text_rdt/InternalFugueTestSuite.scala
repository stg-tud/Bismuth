package text_rdt

import munit.FunSuite
import scala.concurrent.duration.Duration

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
      ReplicaState("A")(using
        ComplexFugueFactory.complexFugueFactory
      )
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.factory.tree.size, 2)
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
      ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.factory.tree.size, 2)
  }

}

abstract class InternalFugueTestSuite(
    val factoryConstructor: () => FugueFactory
) extends FunSuite {

  test("regression-1") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, '/')
    assertEquals(replicaStateA.text(), "/")
    replicaStateA.insert(0, 'e')
    assertEquals(replicaStateA.text(), "e/")
    replicaStateA.insert(0, '{')
    assertEquals(replicaStateA.text(), "{e/")
  }

  test("regression-2") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, '9')
    replicaStateA.insert(1, 'F')

    replicaStateA.insert(1, 'e')
    replicaStateA.insert(0, '5')
    assertEquals(replicaStateA.text(), "59eF")
  }

  test("regression-3") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'n')
    replicaStateA.insert(1, 'T')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, '<')
    assertEquals(replicaStateA.text(), "n<3T")
  }

  test("regression-4") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'v')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, 'G')
    assertEquals(replicaStateA.text(), "vG3")
    replicaStateA.insert(3, 'B')
    assertEquals(replicaStateA.text(), "vG3B")
  }

  test("regression-5") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'v')
    assertEquals(replicaStateA.text(), "v")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "")
  }

  test("regression-6") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'M')
    replicaStateA.insert(1, 'T')
    assertEquals(replicaStateA.text(), "MT")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "M")
  }

  test("regression-7") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'h')
    replicaStateA.insert(1, 'p')
    assertEquals(replicaStateA.text(), "hp")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "p")
  }

  test("regression-8") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, ']')
    replicaStateA.insert(1, '8')
    replicaStateA.insert(2, 'o')
    assertEquals(replicaStateA.text(), "]8o")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "]o")
  }

  test("regression-9") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
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
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'h')
    replicaStateA.insert(2, ',')
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "/,")
  }

  test("regression-11") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, '/')
    assertEquals(replicaStateA.text(), "/")
    assertEquals(replicaStateB.text(), "")
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "/")
    assertEquals(replicaStateB.text(), "/")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "")
    assertEquals(replicaStateB.text(), "/")
    replicaStateB.delete(0)
    assertEquals(replicaStateA.text(), "")
    assertEquals(replicaStateB.text(), "")
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "")
    assertEquals(replicaStateB.text(), "")
  }

  test("regression-12") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, '$')
    replicaStateA.insert(1, 'c')
    replicaStateA.insert(1, 'B')
    replicaStateA.delete(0)
    replicaStateA.insert(1, 'e')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "Bec")
    assertEquals(replicaStateB.text(), "Bec")
  }

  test("regression-13") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateB.insert(0, 'O')
    replicaB.sync(replicaA)
    replicaStateB.insert(1, '4')
    replicaStateA.insert(1, '{')
    replicaB.sync(replicaA)
    assertEquals(
      replicaStateA.text(),
      replicaStateB.text()
    )
  }

  test("regression-14") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateB.insert(0, 'L')
    assertEquals(replicaStateB.text(), "L")
    replicaStateB.insert(1, 'L')
    assertEquals(replicaStateB.text(), "LL")
    replicaStateB.insert(2, '/')
    assertEquals(replicaStateB.text(), "LL/")
    replicaStateB.insert(3, 'A')
    assertEquals(replicaStateB.text(), "LL/A")
    replicaStateB.insert(2, 'y')
    assertEquals(replicaStateB.text(), "LLy/A")
    replicaStateB.delete(0)
    assertEquals(replicaStateB.text(), "Ly/A")
    replicaStateB.insert(4, 'A')
    assertEquals(replicaStateB.text(), "Ly/AA")
    replicaB.sync(replicaA)
  }

  test("regression-15") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "AB")
    assertEquals(replicaStateB.text(), "AB")
  }

  test("regression-16") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "BA")
    assertEquals(replicaStateB.text(), "BA")
  }

  test("regression-17") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaStateB.insert(1, 'b')
    replicaB.sync(replicaA)
    assertEquals(replicaStateA.text(), "ABb")
    assertEquals(replicaStateB.text(), "ABb")
  }

  test("regression-18") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaStateB.insert(1, 'b')
    replicaB.syncFrom(replicaA.state)
    assertEquals(replicaStateB.text(), "BbA")
  }

  test("regression-19") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaA.syncFrom(replicaStateB)
  }

  test("regression-20") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'a')
    replicaB.syncFrom(replicaStateA)
    replicaStateA.insert(0, 'A')
    replicaStateB.insert(0, 'B')
    replicaA.syncFrom(replicaStateB)
  }

  test("regression-21") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'a')
    replicaStateA.insert(1, 'c')
    replicaStateA.insert(1, 'b')
    replicaB.sync(replicaA)
  }

  test("regression-22") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'a')
    replicaA.sync(replicaB)
    replicaStateB.delete(0)
    replicaStateA.insert(1, 'c')
    replicaB.syncFrom(replicaStateA)
    assertEquals(replicaB.text(), "c")
  }

  test("regression-23") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    replicaStateA.insert(0, 'A')
    assertEquals(replicaStateA.text(), "A")
    replicaStateA.insert(1, 'B')
    assertEquals(replicaStateA.text(), "AB")
    replicaStateA.insert(0, 'C')
    assertEquals(replicaStateA.text(), "CAB")
    replicaStateA.insert(2, 'D')
    assertEquals(replicaStateA.text(), "CADB")
  }

  test("regression-24") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'a')
    replicaStateA.insert(1, 'b')
    replicaStateA.insert(2, 'c')
    replicaA.sync(replicaB)
    replicaStateB.insert(2, 'd')
    replicaStateA.delete(2)
    replicaStateA.delete(1)
    replicaStateA.delete(0)
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "d")
    assertEquals(replicaB.text(), "d")
  }

  test("regression-25") {
    val replicaStateA =
      ReplicaState("B")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("A")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'O')
    replicaStateA.insert(1, 'V')
    replicaStateA.insert(2, 'Z')
    replicaA.sync(replicaB)
    replicaStateB.insert(2, '>')
    replicaStateA.delete(0)
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "V>Z")
    assertEquals(replicaB.text(), "V>Z")
  }

  test("regression-26") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'm')
    replicaStateA.insert(1, ' ')
    replicaStateA.delete(1)
    replicaA.sync(replicaB)
  }

  test("2023-weidner-minimizing-interleaving-figure-1") {
    val replicaStateA =
      ReplicaState("A")(using factoryConstructor())
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using factoryConstructor())
    val replicaB = Replica(replicaStateB, NoopEditory())
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "")
    replicaA.state.insert(0, 'm')
    replicaA.state.insert(1, 'i')
    replicaA.state.insert(2, 'l')
    replicaA.state.insert(3, 'k')
    replicaA.state.insert(4, '\n')
    assertEquals(replicaA.text(), "milk\n")
    assertEquals(replicaB.text(), "")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milk\n")
    assertEquals(replicaB.text(), "milk\n")
    replicaA.state.insert(4, '\n')
    replicaA.state.insert(5, 'e')
    replicaA.state.insert(6, 'g')
    replicaA.state.insert(7, 'g')
    replicaA.state.insert(8, 's')
    assertEquals(replicaA.text(), "milk\neggs\n")
    assertEquals(replicaB.text(), "milk\n")
    replicaB.state.insert(4, '\n')
    replicaB.state.insert(5, 'b')
    replicaB.state.insert(6, 'r')
    replicaB.state.insert(7, 'e')
    replicaB.state.insert(8, 'a')
    replicaB.state.insert(9, 'd')
    assertEquals(replicaA.text(), "milk\neggs\n")
    assertEquals(replicaB.text(), "milk\nbread\n")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milk\neggs\nbread\n")
    assertEquals(replicaB.text(), "milk\neggs\nbread\n")
  }

}
