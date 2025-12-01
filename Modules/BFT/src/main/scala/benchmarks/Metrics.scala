package benchmarks

case class Metrics(roundtrips: Int, byteSent: Int)

case class Measurement(
                        runId: Int,
                        method: String,
                        dagSize: Int,
                        diffSize: Int,
                        roundTrips: Int,
                        bandwidth: Int
                      )

object Measurement:
  def writeCSVRow(file: java.io.FileWriter, m: Measurement): Unit = {
    file.write(
      s"${m.runId},${m.method},${m.dagSize},${m.diffSize},${m.roundTrips},${m.bandwidth}\n"
    )
  }