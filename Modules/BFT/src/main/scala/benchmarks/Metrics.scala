package benchmarks

import java.io.PrintWriter

case class Metrics(roundtrips: Int, byteSent: Int)

case class Measurement(
    method: String = "",
    dagSize: Int = -1,
    diff: Float = -1,
    roundTrips: Int = -1,
    bandwidth: Long = -1,
    delta: Long = -1,
    codedSymbolPerRoundTrip: Int = -1,
    deltaSize: Int = -1,
    falsePositiveRate: Float = -1,
    bloomFilterFailed: Int = -1
):
    override def toString: String =
      s"$method,$dagSize,$diff,$roundTrips,$bandwidth,$delta,$codedSymbolPerRoundTrip,$deltaSize,$falsePositiveRate,$bloomFilterFailed"

object Measurement:
    def writeCSVRows(file: java.io.FileWriter, measurements: Seq[Measurement]): Unit = {
      val printer = new PrintWriter(file)

      val data = measurements.foldLeft("")((acc, m) => acc.concat("\n" + m.toString))
      printer.print(data)

      printer.close()
    }
