package ex201x.basic.meanwindow

import reactives.default.*

import scala.collection.immutable.LinearSeq

object EventWindows {

  def main(args: Array[String]): Unit = {
    val e: Evt[Double] = Evt[Double]()
    // val all = e.list()
    val window: Signal[LinearSeq[Double]] = e.list(5)
    val mean: Signal[Double]              = Signal {
      window.value.sum /
        window.value.length
    }
    mean.changed observe { println(_) }

    e.fire(2)
    e.fire(1)
    e.fire(3)
    e.fire(4)
    e.fire(1)
    e.fire(1)
  }


}
