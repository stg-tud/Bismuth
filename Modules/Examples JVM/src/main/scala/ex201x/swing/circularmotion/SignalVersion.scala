package ex201x.swing.circularmotion

import reactives.default.*

object SignalVersion extends App {

  // Time and radius change over time
  val time: Var[Int]      = Var(0)
  val radius: Signal[Int] = Signal { time.value % 10 } // The circle expands periodically

  // Constants in uniform motion
  val rotationPeriod          = 20
  val angularVelocity: Double = 2 * 3.14 / rotationPeriod

  val speed: Signal[Double]        = Signal { angularVelocity * radius.value }
  val angle: Signal[Double]        = Signal { ((angularVelocity * time.value / 3.14) * 180) % 360 }
  val acceleration: Signal[Double] = Signal { angularVelocity * angularVelocity * radius.value }
  val space: Signal[Double]        = Signal { speed.value * time.value }

  // Print all the results.
  // Note that the order in which the items are printed is not deterministic.
  radius.changed observe { x => print(f"Radius: $x%d  ") }
  speed.changed observe { x => print(f"Speed: $x%.2f  ") }
  angle.changed observe { x => print(f"Angle: $x%.2f  ") }
  acceleration.changed observe { x => print(f"Acceleration: $x%.2f  ") }
  space.changed observe { x => println(f"Space: $x%.2f  ") }

  while true do {
    Thread `sleep` 200
    time.transform(_ + 1)
  }

}
