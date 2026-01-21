package ex2016universe

import reactives.default.*

class Time {
  val tick: Evt[Unit] = Evt[Unit]()

  val hours: Signal[Int]         = tick.count()
  val day: Signal[Int]           = hours `map` (_ / 24)
  val hour: Signal[Int]          = hours `map` (_ % 24)
  val week: Signal[Int]          = day `map` (_ / 7)
  val timestring: Signal[String] = Signal(s"Week: ${week.value} Day: ${day.value}  hour: ${hour.value}")
  val newWeek                    = week.changed
  override def toString: String  = timestring.readValueOnce
}
