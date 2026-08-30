package ex2016programmingdemo

case class Pos(x: Double, y: Double) {
  def *(v: Double): Pos = Pos(x * v, y * v)
  def +(v: Pos): Pos    = Pos(x + v.x, y + v.y)
}
