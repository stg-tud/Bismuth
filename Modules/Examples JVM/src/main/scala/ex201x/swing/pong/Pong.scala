package ex201x.swing.pong

import ex201x.Mouse
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

  val x: Signal[Int] = tick.fold(initPosition.x) { (pos, _) => pos + speedX.now }
  val y: Signal[Int] = tick.fold(initPosition.y) { (pos, _) => pos + speedY.now }

  val mouseY: Signal[Int] = Signal { mouse.position.value.getY().toInt }

  val leftRacket  = new Racket(LeftRacketPos, mouseY)
  val rightRacket = new Racket(RightRacketPos, y)

  val rackets: Signal[List[Racket]]         = Signal { List(leftRacket, rightRacket) }
  val areas: Signal[List[Rectangle]]           = Signal.dynamic { rackets.value.map(_.area.value) }
  val ballInRacket: Signal[Boolean]    = Signal { areas.value.exists(_.contains(x.value, y.value)) }
  val collisionRacket: Event[Boolean] = ballInRacket.changed.filter(_ == true)

  val leftWall: Event[Int]  = x.changed && (x => x < 0)
  val rightWall: Event[Int] = x.changed && (x => x + Ball.Size > Pong.Max_X)

  val xBounce: Event[Int | Boolean] = leftWall || rightWall || collisionRacket
  val yBounce: Event[Int] = y.changed && (y => y < 0 || y + Ball.Size > Pong.Max_Y)

  val speedX: Signal[Int] = xBounce.toggle(Var(speed.x), Var(-speed.x))
  val speedY: Signal[Int] = yBounce.toggle(Var(speed.y), Var(-speed.y))

  val pointsPlayer: Signal[Int]   = rightWall.count()
  val pointsComputer: Signal[Int] = leftWall.count()

  val score: Signal[String] = Signal { pointsPlayer.value.toString + " : " + pointsComputer.value }
}
