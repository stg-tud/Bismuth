package benchmarks

import java.io.FileWriter
import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex
import scala.util.matching.Regex
import scala.util.matching.Regex
import scala.util.matching.Regex

object JMHtoCSV {
  val BENCHMARK: Regex   = "^# Benchmark: (.*)$".r
  val THREADS: Regex     = "^# Threads: (\\d+) thread.*$".r
  val PARAMETERS: Regex  = "^# Parameters: \\((.*)\\)$".r
  val MODE: Regex        = "^# Benchmark mode: (.*)$".r
  val MEASUREMENT: Regex = "^Iteration .*: (\\d+)[.,](\\d+) .*?$".r

  var outfiles: Map[String, FileWriter]           = Map[String, FileWriter]()
  var benchmark: String  = scala.compiletime.uninitialized
  var threads: String    = scala.compiletime.uninitialized
  var parameters: String = scala.compiletime.uninitialized
  var mode: String       = scala.compiletime.uninitialized

  def main(args: Array[String]): Unit = {
    try {
      for fileName <- args do {
        println("processing " + fileName)
        for (line, lineNo) <- Source.fromFile(fileName).getLines().zipWithIndex do {
          line match {
            case BENCHMARK(benchmarkName) =>
              benchmark = benchmarkName
            case THREADS(threadCount) =>
              threads = threadCount
            case MODE(modeText) =>
              mode = modeText
            case PARAMETERS(allParams) =>
              parameters = allParams.substring(allParams.indexOf('=') + 2).replaceAll(", [^=]+ = ", "\t")
              if !outfiles.contains(benchmark) then {
                outfiles += benchmark -> new FileWriter(benchmark + ".txt")
                outfiles(benchmark).write("srcfile\tthreads\t" + allParams.substring(
                  0,
                  allParams.lastIndexOf('=')
                ).replaceAll(" = [^=]+, ", "\t") + "\tmode\tmeasurement\n")
              }
            case MEASUREMENT(integer, decimal) =>
              outfiles(benchmark).write(
                fileName + "\t" + threads + "\t" + parameters + "\t" + mode + "\t" + integer + "," + decimal + "\n"
              )
            case _ => // ignore
          }
        }
      }
    } finally
      for writer <- outfiles.values do writer.close()
    println("done, written files:\n" + outfiles.keySet.mkString(".txt\n") + ".txt")
  }
}
