import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_Int, given_Hashable_String, given_Xorable_Int, given_Xorable_String}

import scala.util.Random

class RIBLTTest extends munit.FunSuite:

  private val testSetSize = if isCI then 500 else 10_000

  test("test riblt with ints") {
    val alice = List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val bob   = List[Int](1, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    val enc = RIBLT[Int]()
    for s <- alice do
      enc.addSymbol(s)

    val dec = RIBLT[Int]()
    for s <- bob do
      dec.addSymbol(s)

    var i = 0
    var d = true
    while d do
      val s = enc.produceNextCodedSymbol
      i += 1
      dec.addCodedSymbol(s)
      dec.tryDecode
      if dec.isDecoded then
        d = false

    assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
    assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

    //print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
    //print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
    //print(s"\n$i coded symbols sent")
  }

  test("test riblt with strings") {
    var alice = List[String]()
    var bob = List[String]()

    var j = 0
    for i <- 0 to testSetSize do
      val r = Random().nextDouble()
      if r <= 0.8 then {
        alice = alice :+ i.toString
        bob = bob :+ i.toString
      } else
        j += 1
        val rr = Random().nextDouble()
        if rr <= 0.5 then
          alice = alice :+ i.toString
        else
          bob = bob :+ i.toString


    val enc = RIBLT[String]()
    for s <- alice do
      enc.addSymbol(s)

    val dec = RIBLT[String]()
    for s <- bob do
      dec.addSymbol(s)

    var i = 0
    var d = true
    while d do
      val s = enc.produceNextCodedSymbol
      i += 1
      //println(i)
      //print("\n")
      dec.addCodedSymbol(s)
      dec.tryDecode
      if dec.isDecoded then
        d = false

    //println(s"diff = $j")

    assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
    assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

    //print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
    //print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
    //print(s"\n$i coded symbols sent")
  }

  test("test riblt with strings 2 ") {
    var alice = List[String]()
    var bob = List[String]()

    var j = 0
    for i <- 0 to testSetSize do
      if i % 2 == 0 then {
        alice = alice :+ i.toString
        bob = bob :+ i.toString
      } else {
        if i % 3 == 0 then
          alice = alice :+ i.toString
        else
          bob = bob :+ i.toString
      }


    val enc = RIBLT[String]()
    for s <- alice do
      enc.addSymbol(s)

    val dec = RIBLT[String]()
    for s <- bob do
      dec.addSymbol(s)

    var i = 0
    var d = true
    while d do
      val s = enc.produceNextCodedSymbol
      i += 1
      //println(i)
      dec.addCodedSymbol(s)
      dec.tryDecode
      if dec.isDecoded then
        d = false

    //println(s"diff = $j")

    assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
    assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

    //print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
    //print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
    //print(s"\n$i coded symbols sent")
  }

