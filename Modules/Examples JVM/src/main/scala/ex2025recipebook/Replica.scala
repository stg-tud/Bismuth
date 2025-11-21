package ex2025recipebook

import rdts.base.Historized.MetaDelta
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, LastWriterWins}
import rdts.time.{Dot, Dots}

class Replica[A, D <: DeltaBuffer[A, D]](val replicaId: LocalUid, var state: A, var buffer: DeltaBuffer[A, D]) {
  var dots: Dots = Dots.empty

  def mod(f: LocalUid ?=> A => A)(using Lattice[A]): this.type = {
    val dot = nextDot
    dots = dots.add(dot)

    val delta  = f(using replicaId)(state)
    val dotsId = Dots.single(dot)

    state = state `merge` delta
    buffer = buffer.applyDelta(delta, dotsId)

    this
  }

  def applyDelta(metaDelta: MetaDelta[A])(using Lattice[A]): Unit = {
    if dots.contains(metaDelta.id) then return

    val merged = state `merge` metaDelta.delta
    if state == merged then return

    state = merged
    dots = dots.union(metaDelta.id.union(metaDelta.redundantDots))
    buffer = buffer.applyDelta(metaDelta)
  }

  def produceDelta(f: LocalUid ?=> A => A)(using Lattice[A]): MetaDelta[A] = {
    val dot = nextDot
    dots = dots.add(dot)

    val delta  = f(using replicaId)(state)
    val dotsId = Dots.single(dot)

    state = state `merge` delta
    MetaDelta(dotsId, delta)
  }

  def receive(other: Replica[A, D])(using Lattice[A]): this.type = {
    other.getDeltas(dots).foreach(applyDelta)
    this
  }

  def getDeltas(seen: Dots): Iterable[MetaDelta[A]] = buffer.getDeltas(seen)

  def show[B](select: A => B = identity): Unit = {
    print(s"$replicaId: ")
    pprint.pprintln(select(state))
    println(f"buffer:         ${buffer.getDeltas(Dots.empty).map(md => f"(${md.id},${md.delta})")}")
    println(f"dots:           $dots")
    println(
      f"redundant:      ${dots.subtract(buffer.getDeltas(Dots.empty).foldLeft(Dots.empty)((acc, md) => acc.union(md.id)))}"
    )
  }

  inline def nextDot: Dot = dots.nextDot(using replicaId)

}

object Replica {
  def quiescence[A: {Lattice}, D <: DeltaBuffer[A, D]](replicas: Replica[A, D]*): Unit =
    replicas.toList match
        case Seq() | Seq(_) => ()
        case Seq(a, rem*)   =>
          rem.foreach(a.receive)
          rem.foreach(r => r.receive(a))

  def main(args: Array[String]): Unit =
    ew()

  def ew(): Unit = {
    val random          = new scala.util.Random(123456789)
    val list: List[Int] = List.fill(10)(random.nextInt())

    val deltaBuffer = DeltaBufferNonRedundant[EnableWinsFlag]()
    val replica     = Replica(LocalUid.gen(), EnableWinsFlag.empty, deltaBuffer)

    list.foreach { r =>
      println(s"r: $r ${if r % 2 != 0 then "enable" else "disable"}")
      replica.mod(ew => if (r % 2) != 0 then ew.enable(using replica.replicaId)() else ew.disable())
      replica.show()
    }
  }

  def lww(): Unit = {
    given Bottom[Int]   = Bottom.provide(0)
    val random          = new scala.util.Random(123456789)
    val list: List[Int] = List.fill(10)(random.nextInt())

    val deltaBuffer = DeltaBufferSubsumed[LastWriterWins[Int]]()
    val replica     = Replica(LocalUid.gen(), LastWriterWins.empty, deltaBuffer)

    list.foreach { r =>
      replica.mod(lww => lww.write(r))
      replica.show()
    }
  }

}
