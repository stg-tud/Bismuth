import dag.Event
import datatypes.ORSet
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_Int, given_Hashable_String, given_Xorable_Int, given_Xorable_String}

import java.security.MessageDigest
import scala.concurrent.duration.*
import scala.util.Random

class RIBLTTest extends munit.FunSuite:

    private val testSetSize = if isCI then 500 else 10_000

    override def munitTimeout: Duration = 5.minutes

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

      // print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
      // print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
      // print(s"\n$i coded symbols sent")
    }

    test("test riblt with strings") {
      var alice = List[String]()
      var bob   = List[String]()

      var j = 0
      for i <- 0 to 10000 do
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
          // println(i)
          // print("\n")
          dec.addCodedSymbol(s)
          dec.tryDecode
          if dec.isDecoded then
              d = false

      // println(s"diff = $j")

      assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
      assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

      // print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
      // print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
      // print(s"\n$i coded symbols sent")
    }

    test("test riblt with strings 2 ") {
      var alice = List[String]()
      var bob   = List[String]()

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
          // println(i)
          dec.addCodedSymbol(s)
          dec.tryDecode
          if dec.isDecoded then
              d = false

      // println(s"diff = $j")

      assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
      assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

      // print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
      // print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
      // print(s"\n$i coded symbols sent")
    }

    test("test riblt with strings 3 ") {
      val alice = List[String]("a", "b")
      val bob   = List[String]("a", "c")

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
          // println(i)
          dec.addCodedSymbol(s)
          dec.tryDecode
          if dec.isDecoded then
              d = false

      // println(s"diff = $j")

      assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
      assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

      // print(s"${dec.remoteSymbols.map(s => s.value).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
      // print(s"${dec.localSymbols.map(s => s.value).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
      // print(s"\n$i coded symbols sent")
    }

    test("test") {
      var replica1 = ORSet[String]()
      var replica2 = ORSet[String]()
      val riblt1   = RIBLT[String]()
      val riblt2   = RIBLT[String]()
      var both     = 0
      var a        = 0
      var b        = 0

      var j = 0
      for i <- 0 to 1000 do
          // println(i)
          val r = Random().nextDouble()
          if r <= 0.8 then {
            var e  = ORSet[String]()
            val rr = Random().nextDouble()
            if rr <= 0.5 then
                e = replica1.add(i.toString)
            else
                e = replica2.add(i.toString)

            replica1 = replica1.merge(e)
            replica2 = replica2.merge(e)
            both += 1
          } else
              j += 1
              val rr = Random().nextDouble()
              if rr <= 0.5 then {
                a += 1
                replica1 = replica1.merge(replica1.add(i.toString))
              } else {
                b += 1
                replica2 = replica2.merge(replica2.add(i.toString))
              }

      var r = 0

      // println(s"similarity $both")
      // println(s"diff $j")
      // println(s"unique to alice $a")
      // println(s"alice all  ${replica1.hashDAG.getIDs.size}")
      // println(s"unique to bob $b")
      // println(s"bob all ${replica2.hashDAG.getIDs.size}")

      for id <- replica1.hashDAG.getIDs do
          riblt1.addSymbol(id)

      for id <- replica2.hashDAG.getIDs do
          riblt2.addSymbol(id)

      var i = 0
      var d = true
      while d do
          val s = riblt1.produceNextCodedSymbol
          i += 1
          // println(i)
          // print("\n")
          riblt2.addCodedSymbol(s)
          riblt2.tryDecode
          if riblt2.isDecoded then
              d = false
    }

    test("test riblt using Event IDs as source symbols") {
      var alice = List[String]()
      var bob   = List[String]()

      val m    = MessageDigest.getInstance("SHA3-512")
      var both = 0
      var a    = 0
      var b    = 0

      var j = 0
      for i <- 0 to 1000 do
          val r = Random().nextDouble()
          if r <= 0.8 then {
            alice = alice :+ m.digest(i.toString.getBytes).mkString("Array(", ", ", ")")
            bob = bob :+ m.digest(i.toString.getBytes).mkString("Array(", ", ", ")")
            both += 1
          } else
              j += 1
              val rr = Random().nextDouble()
              if rr <= 0.5 then {
                alice = alice :+ m.digest(i.toString.getBytes).mkString("Array(", ", ", ")")
                a += 1
              } else {
                bob = bob :+ m.digest(i.toString.getBytes).mkString("Array(", ", ", ")")
                b += 1
              }

      // println(s"similarity $both")
      // println(s"diff $j")
      // println(s"unique to alice $a")
      // println(s"alice all  ${alice.size}")
      // println(s"unique to bob $b")
      // println(s"bob all ${bob.size}")

      // for id <- alice do
      //    println(id)

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
          // print("\n")
          dec.addCodedSymbol(s)
          dec.tryDecode
          if dec.isDecoded then
              d = false

      // println(s"diff = $j")

      assertEquals(alice.toSet -- bob.toSet, dec.remoteSymbols.map(s => s.value).toSet)
      assertEquals(bob.toSet -- alice.toSet, dec.localSymbols.map(s => s.value).toSet)

      // print(s"${dec.remoteSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Alice")
      // print(s"${dec.localSymbols.map(s => s.symbol).fold("")((s1, s2) => s"$s1, $s2")} is exclusive to Bob")
      // print(s"\n$i coded symbols sent")
    }
