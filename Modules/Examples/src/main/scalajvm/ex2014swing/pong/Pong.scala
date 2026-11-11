package ex2014swing.pong

import ex2014swing.Mouse
import reactives.default.*

import java.awt.{Point, Rectangle}

object Pong {
  val Max_X = 800
  val Max_Y = 400
}

object Ball {
  val Size = 20
}

class Pong(val tick: Evt[Unit], val mouse: Mouse) {

  val LeftRacketPos  = 30
  val RightRacketPos = 770

  val initPosition = new Point(400, 200)
  val speed        = new Point(10, 8)

  val mouseY: Signal[Int] = Signal { mouse.position.value.getY().toInt }

  /** Ball position, rackets, and score depend on each other. The cycle is defined with local lazy vals,
    * because class fields would be accessed before they are initialized.
    */
  private def game() = {
    lazy val x: Signal[Int] = tick.fold(initPosition.x) { (pos, _) => pos + speedX.now }
    lazy val y: Signal[Int] = tick.fold(initPosition.y) { (pos, _) => pos + speedY.now }

    lazy val leftRacket  = new Racket(LeftRacketPos, mouseY)
    lazy val rightRacket = new Racket(RightRacketPos, y)

    lazy val rackets: Signal[List[Racket]]   = Signal { List(leftRacket, rightRacket) }
    lazy val areas: Signal[List[Rectangle]]  = Signal.dynamic { rackets.value.map(_.area.value) }
    lazy val ballInRacket: Signal[Boolean]   = Signal { areas.value.exists(_.contains(x.value, y.value)) }
    lazy val collisionRacket: Event[Boolean] = ballInRacket.changed.filter(_ == true)

    lazy val leftWall: Event[Int]  = x.changed && (x => x < 0)
    lazy val rightWall: Event[Int] = x.changed && (x => x + Ball.Size > Pong.Max_X)

    lazy val xBounce: Event[Int | Boolean] = leftWall || rightWall || collisionRacket
    lazy val yBounce: Event[Int]           = y.changed && (y => y < 0 || y + Ball.Size > Pong.Max_Y)

    lazy val speedX: Signal[Int] = xBounce.toggle(Var(speed.x), Var(-speed.x))
    lazy val speedY: Signal[Int] = yBounce.toggle(Var(speed.y), Var(-speed.y))

    val pointsPlayer: Signal[Int]   = rightWall.count()
    val pointsComputer: Signal[Int] = leftWall.count()

    val score: Signal[String] = Signal { pointsPlayer.value.toString + " : " + pointsComputer.value }
    (x, y, leftRacket, rightRacket, score)
  }

  private val network = game()

  val x: Signal[Int]        = network._1
  val y: Signal[Int]        = network._2
  val leftRacket: Racket    = network._3
  val rightRacket: Racket   = network._4
  val score: Signal[String] = network._5
}
