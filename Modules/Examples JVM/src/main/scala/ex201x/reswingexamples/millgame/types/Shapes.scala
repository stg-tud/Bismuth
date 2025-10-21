package ex201x.reswingexamples.millgame.types

import java.awt.Color
import scala.Numeric.Implicits.*

//
// presentational attributes
//
case class Presentation[T, +S](
    shape: S,
    color: Color = Color.BLACK,
    width: T = 1
)(implicit ev$1: S => Shape[T], ev$2: Numeric[T]) {
  def toDouble: Presentation[Double, Shape[Double]] = Presentation(ev$1(shape).toDouble, color, width.toDouble)
  def toInt: Presentation[Int, Shape[Int]]          = Presentation(ev$1(shape).toInt, color, width.toInt)
}

//
// shape
//
sealed abstract class Shape[T] {
  def toDouble: Shape[Double]
  def toInt: Shape[Int]
}

//
// point
//
object Point {
  implicit def fromPoint[T](p: Point[T]): (T, T)        = (p.x, p.y)
  implicit def toPoint[T: Numeric](p: (T, T)): Point[T] = Point(p._1, p._2)
}

case class Point[@specialized(Int, Double) T: Numeric](x: T, y: T) extends Shape[T] {
  def toDouble: Point[Double] = Point(x.toDouble, y.toDouble)
  def toInt: Point[Int]       = Point(x.toInt, y.toInt)

  def +(p: Point[T]): Point[T]    = Point(x + p.x, y + p.y)
  def -(p: Point[T]): Point[T]    = Point(x - p.x, y - p.y)
  def unary_- : Point[T]          = Point(-x, -y)
  def *(d: T): Point[T]           = Point(x * d, y * d)
  def /(d: Double): Point[Double] = Point(x.toDouble / d, y.toDouble / d)

  def euclidian: Double                               = math.sqrt((x * x + y * y).toDouble)
  def normalize: Point[? >: T & Double <: T | Double] = if x == 0 && y == 0 then this else this / euclidian
  def distance(p: Point[T]): Double                   = (p - this).euclidian
  def directionTo(p: Point[T]): Point[? >: T & Double <: T | Double] = (p - this).normalize
}

//
// line
//
object Line {
  implicit def fromLine[T](l: Line[T]): (Point[T], Point[T]) = (l.from, l.to)
  implicit def toLine[T, A, B](l: (A, B))(implicit
      ev$1: A => Point[T],
      ev$2: B => Point[T],
  ): Line[T] =
    Line(ev$1(l._1), ev$2(l._2))
}

case class Line[T](from: Point[T], to: Point[T]) extends Shape[T] {
  def toDouble: Shape[Double] = Line(from.toDouble, to.toDouble)
  def toInt: Shape[Int]       = Line(from.toInt, to.toInt)

  def length: Double = from `distance` to
}

//
// rectangle
//
object Rect {
  implicit def apply[T: Numeric](x: T, y: T, width: T, height: T): Rect[T] =
    Rect(Point(x, y), width, height)
}

case class Rect[T: Numeric](anchor: Point[T], width: T, height: T) extends Shape[T] {
  def toDouble: Shape[Double] = Rect(anchor.toDouble, width.toDouble, height.toDouble)
  def toInt: Shape[Int]       = Rect(anchor.toInt, width.toInt, height.toInt)
}

//
// circle
//
object Circle {
  implicit def apply[T: Numeric](x: T, y: T, radius: T): Circle[T] =
    Circle(Point(x, y), radius)
}

case class Circle[T: Numeric](center: Point[T], radius: T) extends Shape[T] {
  def toDouble: Shape[Double] = Circle(center.toDouble, radius.toDouble)
  def toInt: Shape[Int]       = Circle(center.toInt, radius.toInt)
}
