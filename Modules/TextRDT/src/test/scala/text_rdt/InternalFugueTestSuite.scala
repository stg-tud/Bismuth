package text_rdt

import munit.FunSuite

class SimpleInternalFugueTestSuite
    extends InternalFugueTestSuite(replicaId => Replica(ReplicaState(replicaId)(using SimpleFugueFactory.simpleFugueFactory), StringEditory())) {}

class ComplexInternalFugueTestSuite
    extends InternalFugueTestSuite(replicaId => Replica(ReplicaState(replicaId)(using ComplexFugueFactory.complexFugueFactory), StringEditory())) {
  test("batching-works") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.state.factory.tree.size, 2)
  }
}

class SimpleAVLInternalFugueTestSuite
    extends InternalFugueTestSuite(replicaId => Replica(ReplicaState(replicaId)(using SimpleAVLFugueFactory.simpleAVLFugueFactory), StringEditory())) {}

class ComplexAVLInternalFugueTestSuite
    extends InternalFugueTestSuite(replicaId => Replica(ReplicaState(replicaId)(using ComplexAVLFugueFactory.complexAVLFugueFactory), StringEditory())) {
  
  test("batching-works") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'e')
    replicaStateA.insert(2, 'e')
    assertEquals(replicaStateA.state.factory.tree.size, 2)
  }

  // TODO put this into a general fugue subclass
  test("regression-28") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
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
}

class OTAlgorithmTestSuite extends InternalFugueTestSuite(replicaId => OTAlgorithm(replicaId, Vector.empty))

abstract class InternalFugueTestSuite[A](
    val factoryConstructor: String => A
)(using algorithm: CollaborativeTextEditingAlgorithm[A]) extends FunSuite {

  test("regression-1") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, '/')
    assertEquals(replicaStateA.text(), "/")
    replicaStateA.insert(0, 'e')
    assertEquals(replicaStateA.text(), "e/")
    replicaStateA.insert(0, '{')
    assertEquals(replicaStateA.text(), "{e/")
  }

  test("regression-2") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, '9')
    replicaStateA.insert(1, 'F')

    replicaStateA.insert(1, 'e')
    replicaStateA.insert(0, '5')
    assertEquals(replicaStateA.text(), "59eF")
  }

  test("regression-3") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, 'n')
    replicaStateA.insert(1, 'T')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, '<')
    assertEquals(replicaStateA.text(), "n<3T")
  }

  test("regression-4") {
    val replicaStateA = factoryConstructor("A")
    replicaStateA.insert(0, 'v')
    replicaStateA.insert(1, '3')
    replicaStateA.insert(1, 'G')
    assertEquals(replicaStateA.text(), "vG3")
    replicaStateA.insert(3, 'B')
    assertEquals(replicaStateA.text(), "vG3B")
  }

  test("regression-5") {
    val replicaStateA =
      factoryConstructor("A")
    replicaStateA.insert(0, 'v')
    assertEquals(replicaStateA.text(), "v")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "")
  }

  test("regression-6") {
    val replicaStateA =
      factoryConstructor("A")
    replicaStateA.insert(0, 'M')
    replicaStateA.insert(1, 'T')
    assertEquals(replicaStateA.text(), "MT")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "M")
  }

  test("regression-7") {
    val replicaStateA =
      factoryConstructor("A")
    replicaStateA.insert(0, 'h')
    replicaStateA.insert(1, 'p')
    assertEquals(replicaStateA.text(), "hp")
    replicaStateA.delete(0)
    assertEquals(replicaStateA.text(), "p")
  }

  test("regression-8") {
    val replicaStateA =
      factoryConstructor("A")
    replicaStateA.insert(0, ']')
    replicaStateA.insert(1, '8')
    replicaStateA.insert(2, 'o')
    assertEquals(replicaStateA.text(), "]8o")
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "]o")
  }

  test("regression-9") {
    val replicaStateA =
      factoryConstructor("A")
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
      factoryConstructor("A")
    replicaStateA.insert(0, '/')
    replicaStateA.insert(1, 'h')
    replicaStateA.insert(2, ',')
    replicaStateA.delete(1)
    assertEquals(replicaStateA.text(), "/,")
  }

  test("regression-11") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
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
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, '$')
    replicaA.insert(1, 'c')
    replicaA.insert(1, 'B')
    replicaA.delete(0)
    replicaA.insert(1, 'e')
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "Bec")
    assertEquals(replicaB.text(), "Bec")
  }

  test("regression-13") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaB.insert(0, 'O')
    replicaB.sync(replicaA)
    replicaB.insert(1, '4')
    replicaA.insert(1, '{')
    replicaB.sync(replicaA)
    assertEquals(
      replicaA.text(),
      replicaB.text()
    )
  }

  test("regression-14") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaB.insert(0, 'L')
    assertEquals(replicaB.text(), "L")
    replicaB.insert(1, 'L')
    assertEquals(replicaB.text(), "LL")
    replicaB.insert(2, '/')
    assertEquals(replicaB.text(), "LL/")
    replicaB.insert(3, 'A')
    assertEquals(replicaB.text(), "LL/A")
    replicaB.insert(2, 'y')
    assertEquals(replicaB.text(), "LLy/A")
    replicaB.delete(0)
    assertEquals(replicaB.text(), "Ly/A")
    replicaB.insert(4, 'A')
    assertEquals(replicaB.text(), "Ly/AA")
    replicaB.sync(replicaA)
  }

  test("regression-15") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "AB")
    assertEquals(replicaB.text(), "AB")
  }

  test("regression-16") {
    val replicaA = factoryConstructor("B")
    val replicaB = factoryConstructor("A")
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "BA")
    assertEquals(replicaB.text(), "BA")
  }

  test("regression-17") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.insert(1, 'b')
    replicaB.sync(replicaA)
    assertEquals(replicaA.text(), "ABb")
    assertEquals(replicaB.text(), "ABb")
  }

  test("regression-18") {
    val replicaA = factoryConstructor("B")
    val replicaB = factoryConstructor("A")
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaB.insert(1, 'b')
    replicaB.syncFrom(replicaA)
    assertEquals(replicaB.text(), "BbA")
  }

  test("regression-19") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaA.syncFrom(replicaB)
  }

  test("regression-20") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'a')
    replicaB.syncFrom(replicaA)
    replicaA.insert(0, 'A')
    replicaB.insert(0, 'B')
    replicaA.syncFrom(replicaB)
  }

  test("regression-21") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'a')
    replicaA.insert(1, 'c')
    replicaA.insert(1, 'b')
    replicaB.sync(replicaA)
  }

  test("regression-22") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'a')
    replicaA.sync(replicaB)
    replicaB.delete(0)
    replicaA.insert(1, 'c')
    replicaB.syncFrom(replicaA)
    assertEquals(replicaB.text(), "c")
  }

  test("regression-23") {
    val replicaA = factoryConstructor("A")
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
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
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
    val replicaA = factoryConstructor("B")
    val replicaB = factoryConstructor("A") 
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
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'm')
    replicaA.insert(1, ' ')
    replicaA.delete(1)
    replicaA.sync(replicaB)
  }

  test("regression-27") {
    val replicaA = factoryConstructor("D") 
    val replicaC = factoryConstructor("C")
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
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, '4')
    replicaA.delete(0)
    replicaB.insert(0, '@')
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "@")
    assertEquals(replicaB.text(), "@")
  }

  test("regression-29") {
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, '4')
    replicaA.insert(0, '0')
    replicaB.insert(0, '@')
    replicaA.sync(replicaB)
    assertEquals(replicaB.text(), "04@")
    assertEquals(replicaA.text(), "04@")
  }

  test("regression-30") {
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'c')
    replicaA.insert(1, '0')
    replicaB.insert(0, 'K')
    replicaA.sync(replicaB)
    assertEquals(replicaB.text(), "c0K")
    assertEquals(replicaA.text(), "c0K")
  }

  test("regression-31") {
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    replicaA.insert(0, 'c')
    replicaA.insert(1, '0')
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "c0")
    assertEquals(replicaB.text(), "c0")
  }

  test("regression-32") {
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    val replicaC = factoryConstructor("C")
    replicaB.insert(0, 'A')
    replicaA.sync(replicaB)
    replicaA.delete(0)
    replicaA.sync(replicaC)
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "A")
    assertEquals(replicaC.text(), "")
  }

  test("regression-33") {
    val replicaA = factoryConstructor("A") 
    val replicaB = factoryConstructor("B")
    val replicaC = factoryConstructor("C")
    replicaA.insert(0, 'u')
    replicaB.insert(0, '=')
    replicaA.delete(0)
    replicaA.sync(replicaC)
    replicaB.sync(replicaC)
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "=")
    assertEquals(replicaC.text(), "=")
  }

  test("regression-34") {
    val replicaA = factoryConstructor("C") 
    val replicaB = factoryConstructor("B")
    val replicaC = factoryConstructor("A")
    replicaC.insert(0, '}')
    replicaA.sync(replicaC)
    assertEquals(replicaA.text(), replicaC.text())
    replicaA.insert(1, 'Q')
    replicaC.delete(0)
    replicaA.insert(0, '>')
    replicaA.sync(replicaC)
    assertEquals(replicaC.text(), replicaA.text())
    replicaB.sync(replicaC)
    assertEquals(replicaB.text(), replicaC.text())
  }

  /* If an operation O is
  transformed against the same group of context-independent
  operations in multiple invocations to transform(O, CD), this
  group of operations must be included in CD and sorted in
  the same total order. Therefore, O can never be transformed
  against the same group of operations in different orders, thus
  breaking PC-CP2. https://dl.acm.org/doi/pdf/10.1145/1180875.1180918 */
  test("regression-35") {
    assume(this.getClass().getName() != "text_rdt.OTAlgorithmTestSuite")
    val replicaA = factoryConstructor("1863976979") 
    val replicaB = factoryConstructor("320716343")
    val replicaC = factoryConstructor("2135938835")
    replicaB.insert(0, '$')
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "$")
    assertEquals(replicaB.text(), "$")
    replicaB.delete(0)
    replicaA.insert(1, 'l')
    replicaC.insert(0, 'E')
    replicaB.sync(replicaA)
    replicaA.sync(replicaC)
    assertEquals(replicaA.text(), replicaC.text())
  }

  test("2023-weidner-minimizing-interleaving-figure-1") {
    val replicaA = factoryConstructor("A")
    val replicaB = factoryConstructor("B")
    assertEquals(replicaA.text(), "")
    assertEquals(replicaB.text(), "")
    replicaA.insert(0, 'm')
    replicaA.insert(1, 'i')
    replicaA.insert(2, 'l')
    replicaA.insert(3, 'k')
    replicaA.insert(4, 'M')
    assertEquals(replicaA.text(), "milkM")
    assertEquals(replicaB.text(), "")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milkM")
    assertEquals(replicaB.text(), "milkM")
    replicaA.insert(4, 'E')
    replicaA.insert(5, 'e')
    replicaA.insert(6, 'g')
    replicaA.insert(7, 'g')
    replicaA.insert(8, 's')
    assertEquals(replicaA.text(), "milkEeggsM")
    assertEquals(replicaB.text(), "milkM")
    replicaB.insert(4, 'B')
    replicaB.insert(5, 'b')
    replicaB.insert(6, 'r')
    replicaB.insert(7, 'e')
    replicaB.insert(8, 'a')
    replicaB.insert(9, 'd')
    assertEquals(replicaA.text(), "milkEeggsM")
    assertEquals(replicaB.text(), "milkBbreadM")
    replicaA.sync(replicaB)
    assertEquals(replicaA.text(), "milkEeggsBbreadM")
    assertEquals(replicaB.text(), "milkEeggsBbreadM")
  }

}
