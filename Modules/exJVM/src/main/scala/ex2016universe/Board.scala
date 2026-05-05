package ex2016universe

import reactives.default.*

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.Map
import scala.util.Random

object Board {
  def proximity(pos: Pos, range: Int): IndexedSeq[Pos] = square(range).map(pos + _).sortBy(pos.distance)
  def square(range: Int): IndexedSeq[Pos]              = for x <- -range to range; y <- -range to range yield Pos(x, y)
}

/** Mutable data structure which stores board elements in 2-dimensional coordinates.
  * A Board is infinite, but width and height specify the area being displayed.
  */
class Board(val width: Int, val height: Int) {
  val allPositions: Set[Pos]             = (for x <- 0 to width; y <- 0 to height yield Pos(x, y)).toSet
  val elementSpawned: Evt[BoardElement]  = Evt[BoardElement]()
  val elementRemoved: Evt[BoardElement]  = Evt[BoardElement]()
  val animalSpawned: Event[BoardElement] = elementSpawned && (_.isAnimal)
  val animalRemoved: Event[BoardElement] = elementRemoved && (_.isAnimal)
  val animalsBorn: Signal[Int]           = animalSpawned.count()
  val animalsDied: Signal[Int]           = animalRemoved.count()
  val animalsAlive: Signal[Int]          = Signal.lift(animalsBorn, animalsDied) { _ - _ }
  var elements: Map[Pos, BoardElement]   = scala.collection.concurrent.TrieMap()

  /** adds a board element at given position */
  def add(be: BoardElement, pos: Pos): Unit = {
    elements.put(pos, be)
    elementSpawned.fire(be)
  }

  /** removes the board element if present in the board */
  def removeDead(): Unit = {
    elements = elements.filter {
      case (p, be) =>
        if be.isDead.readValueOnce then {
          elementRemoved.fire(be)
          false
        } else true
    }
  }

  /** @return the immediate neighbors of the given position */
  def neighbors(pos: Pos): IndexedSeq[BoardElement] = nearby(pos, 1)

  /** @return the elements in this board nearby pos */
  def nearby(pos: Pos, range: Int): IndexedSeq[BoardElement] = Board.proximity(pos, range).flatMap(elements.get)

  /** @return the nearest free position to pos */
  def nearestFree(pos: Pos): Option[Pos] = Board.proximity(pos, 1).find(isFree)

  /** @return true if pos is free */
  def isFree(pos: Pos): Boolean = !elements.contains(pos)

  /** moves pos in direction dir if possible (when target is free) */
  def moveIfPossible(pos: Pos, dir: Pos): Unit = {
    val newPos = pos + dir
    if isFree(newPos) && !isFree(pos) then {
      val e = clear(pos)
      elements.put(newPos, e.get)
      ()
    }
  }

  /** clears the current element from pos */
  private def clear(pos: Pos): Option[BoardElement] = elements.remove(pos)

  /** @return the position of the given BoardElement. slow. */
  def getPosition(be: BoardElement): Option[Pos] = {
    elements.collectFirst {
      case (pos, b) if b == be => pos
    }
  }

  /** @return a random free position on this board */
  def randomFreePosition(random: Random): Pos = {
    val possiblePositions = allPositions.diff(elements.keySet).toVector
    possiblePositions(random.nextInt(possiblePositions.length))
  }

  /** @return textual representation for drawing this board to console */
  def dump: String = {
    def repr(be: Option[BoardElement]) =
      be match {
        case None                                       => '.'
        case Some(m: Male) if m.isAdult.readValueOnce   => 'm'
        case Some(f: Female) if f.isAdult.readValueOnce => if f.isPregnant.readValueOnce then 'F' else 'f'
        case Some(x: Animal)                            => 'x'
        case Some(p: Plant)                             => '#'
        case Some(_)                                    => '?'
      }
    val lines =
      for y <- 0 to height
      yield (0 to width).map(x => repr(elements.get(Pos(x, y)))).mkString
    lines.mkString("\n")
  }
}
