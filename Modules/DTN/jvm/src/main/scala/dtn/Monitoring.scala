package dtn

import io.bullet.borer.{Codec, Json}
import io.bullet.borer.derivation.MapBasedCodecs.*
import rdts.base.Lattice.syntax
import rdts.time.Dots

import java.io.BufferedOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, ZoneId, ZonedDateTime}
import scala.util.Using

class MonitoringPaths(base_dir: String = "/shared/monitoring") {
  val monitoring_dir: Path                = Paths.get(base_dir)
  val received_data_fp: Path              = monitoring_dir.resolve("received.data")
  val forwarded_data_fp: Path             = monitoring_dir.resolve("forwarded.data")
  val created_and_delivered_data_fp: Path = monitoring_dir.resolve("created_and_delivered.data")
  val ratios_fp: Path                     = monitoring_dir.resolve("ratios.data")
}

class MonitoringServer(server: TCPReadonlyServer, paths: MonitoringPaths = MonitoringPaths()) {
  def run(): Unit = {
    val dir = Paths.get("/shared/monitoring")

    Using.Manager { use =>
      val streamReceived            = use(BufferedOutputStream(Files.newOutputStream(paths.received_data_fp)))
      val streamForwarded           = use(BufferedOutputStream(Files.newOutputStream(paths.forwarded_data_fp)))
      val streamCreatedAndDelivered =
        use(BufferedOutputStream(Files.newOutputStream(paths.created_and_delivered_data_fp)))

      try {
        while true do {
          val (connection, data) = server.queue.take()

          val now: ZonedDateTime = ZonedDateTime.now(ZoneId.of("UTC"))

          // println(s"trying to decode data: ${String(data, StandardCharsets.UTF_8)}")

          Json.decode(data).to[MonitoringMessage].value match
            case m: MonitoringMessage.BundleReceivedAtRouter =>
              streamReceived.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamReceived.write("\n".getBytes())
              streamReceived.flush()
            case m: MonitoringMessage.BundleForwardedAtRouter =>
              streamForwarded.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamForwarded.write("\n".getBytes())
              streamForwarded.flush()
            case m: MonitoringMessage.BundleDeliveredAtClient =>
              streamCreatedAndDelivered.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamCreatedAndDelivered.write("\n".getBytes())
              streamCreatedAndDelivered.flush()
            case m: MonitoringMessage.BundleCreatedAtClient =>
              streamCreatedAndDelivered.write(Json.encode[MonitoringMessage](m.copy(time = Option(now))).toByteArray)
              streamCreatedAndDelivered.write("\n".getBytes())
              streamCreatedAndDelivered.flush()
        }
      } catch {
        case e: Exception =>
          println("monitoring server ran into exception:")
          e.printStackTrace()
      } finally {
        server.stop()
      }
    }.recoverAndLog()
    ()
  }
}
object MonitoringServer {
  def apply(interface: String, port: Int): MonitoringServer = {
    val server = TCPReadonlyServer(interface, port)
    server.start()
    println(s"monitoring server started under $interface:$port")
    new MonitoringServer(server)
  }
}

class MonitoringClient(connection: TCPConnection) extends MonitoringClientInterface {
  override def send(message: MonitoringMessage): Unit = {
    connection.send(Json.encode[MonitoringMessage](message).toByteArray)
  }
}
object MonitoringClient {
  def apply(host: String, port: Int): MonitoringClient = {
    new MonitoringClient(TCPConnection(host, port))
  }
}

class MonitoringBundlesReceivedPrinter(paths: MonitoringPaths = MonitoringPaths()) {
  def run(): Unit = {
    Using(Files.newBufferedReader(paths.received_data_fp, StandardCharsets.UTF_8)) { in =>
      var oldTime: Option[ZonedDateTime] = None
      var messageCount: Long             = 0

      while true do {
        val line = in.readLine()

        if line == null then {
          Thread.sleep(10)
        } else {
          Json.decode(line.getBytes()).to[MonitoringMessage].value match
            case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time)         => ()
            case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => ()
            case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time)   => ()
            case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)          => {
              if oldTime.isEmpty then oldTime = time

              if Duration.between(time.get, oldTime.get).toSeconds() >= 1 then {
                print("\u001b[2J") // clear console screen
                println("Total Messages Received\n")
                println(s"${time.get}: ${messageCount}")
                oldTime = time
                messageCount = 1
              } else {
                messageCount += 1
              }
            }
        }
      }
    }.recoverAndLog()
    ()
  }
}

class MonitoringBundlesForwardedPrinter(paths: MonitoringPaths = MonitoringPaths()) {
  def run(): Unit = {
    Using(Files.newBufferedReader(paths.forwarded_data_fp, StandardCharsets.UTF_8)) { in =>
      var oldTime: Option[ZonedDateTime] = None
      var messageCount: Long             = 0

      while true do {
        val line = in.readLine()

        if line == null then {
          Thread.sleep(10)
        } else {
          Json.decode(line.getBytes()).to[MonitoringMessage].value match
            case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => ()
            case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time)   => ()
            case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)          => ()
            case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time)         => {
              if oldTime.isEmpty then oldTime = time

              if Duration.between(time.get, oldTime.get).toSeconds() >= 1 then {
                print("\u001b[2J") // clear console screen
                println("Total Messages Forwarded\n")
                println(s"${time.get}: ${messageCount}")
                oldTime = time
                messageCount = 1
              } else {
                messageCount += 1
              }
            }
        }
      }
    }.recoverAndLog()
    ()
  }
}

class MonitoringStateDevelopmentPrinter(creationClientId: String, paths: MonitoringPaths = MonitoringPaths()) {
  def run(): Unit = {
    Using(Files.newBufferedReader(paths.created_and_delivered_data_fp, StandardCharsets.UTF_8)) { in =>
      var creationState: Dots                = Dots.empty
      var deliveredStates: Map[String, Dots] = Map()

      // these count anomalies that should not happen!?
      var bundlesCreatedAtOtherNodesCounter: Long = 0
      var bundlesDeliveredAtCreationCounter: Long = 0

      var newestTime: Option[ZonedDateTime] = None

      while true do {
        // not optimal as an EOF can crash the application although the line was just not written in one go
        val line = in.readLine()

        if line == null then {
          Thread.sleep(10)
        } else {
          Json.decode(line.getBytes()).to[MonitoringMessage].value match
            case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)          => ()
            case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time)         => ()
            case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => {
              if clientId == creationClientId then {
                bundlesDeliveredAtCreationCounter += 1
              } else {
                deliveredStates = deliveredStates.merge(Map(clientId -> dots))
              }
              newestTime = time
            }
            case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time) => {
              if clientId != creationClientId then {
                bundlesCreatedAtOtherNodesCounter += 1
              } else {
                creationState = creationState.merge(dots)
              }
              newestTime = time
            }

          val creationStateNum: Double = creationState.size.toDouble

          print("\u001b[2J") // clear console screen
          println(s"Latest State Time: ${newestTime.getOrElse(ZonedDateTime.now())}\n")
          println(s"States Ratio of num-dots (num dots created: ${creationStateNum})")
          for (clientId: String, dots: Dots) <- deliveredStates do {
            val deliveredStateNum: Double = dots.size.toDouble
            val ratio                     = deliveredStateNum / creationStateNum
            println(s"${clientId} |  ratio: ${ratio}, num dots delivered: ${deliveredStateNum}")
          }
          println(s"\nNum bundles created at other nodes: ${bundlesCreatedAtOtherNodesCounter}")
          println(s"Num bundles delivered at creation node: ${bundlesDeliveredAtCreationCounter}")
        }
      }
    }.recoverAndLog()
    ()
  }
}

case class RatioMessage(clientId: String, timestamps: List[ZonedDateTime], ratios: List[Double]) derives Codec

class MonitoringStateDevelopmentToRatioConverter(creationClientId: String, paths: MonitoringPaths = MonitoringPaths()) {
  def run(): Unit = {
    var data: Map[String, Tuple2[List[ZonedDateTime], List[Double]]] = Map()

    Using(Files.newBufferedReader(paths.created_and_delivered_data_fp, StandardCharsets.UTF_8)) { in =>
      var creationState: Dots                = Dots.empty
      var deliveredStates: Map[String, Dots] = Map()

      // these count anomalies that should not happen!?
      var bundlesCreatedAtOtherNodesCounter: Long = 0
      var bundlesDeliveredAtCreationCounter: Long = 0

      var line = in.readLine()
      while line != null do {
        Json.decode(line.getBytes()).to[MonitoringMessage].value match
          case MonitoringMessage.BundleReceivedAtRouter(nodeId, bundleId, time)          => ()
          case MonitoringMessage.BundleForwardedAtRouter(nodeId, bundleId, time)         => ()
          case MonitoringMessage.BundleDeliveredAtClient(clientId, bundleId, dots, time) => {
            if clientId == creationClientId then {
              bundlesDeliveredAtCreationCounter += 1
            } else {
              deliveredStates = deliveredStates.merge(Map(clientId -> dots))

              val ratio = deliveredStates(clientId).size.toDouble / creationState.size.toDouble

              data = data.updatedWith(clientId)(option => {
                option match
                  case None        => Option((List(time.get), List(ratio)))
                  case Some(tuple) => Option((tuple._1 :+ time.get, tuple._2 :+ ratio))
              })
            }
          }
          case MonitoringMessage.BundleCreatedAtClient(clientId, bundleId, dots, time) => {
            if clientId != creationClientId then {
              bundlesCreatedAtOtherNodesCounter += 1
            } else {
              creationState = creationState.merge(dots)

              deliveredStates.foreach((cId, d) => {
                val ratio = d.size.toDouble / creationState.size.toDouble

                data = data.updatedWith(cId)(option => {
                  option match
                    case None        => Option((List(time.get), List(ratio)))
                    case Some(tuple) => Option((tuple._1 :+ time.get, tuple._2 :+ ratio))
                })
              })
            }
          }

        line = in.readLine()
      }
    }.recoverAndLog()

    Using(BufferedOutputStream(Files.newOutputStream(paths.ratios_fp))) { out =>
      {
        data.foreach((clientId, tuple) => {
          val message = RatioMessage(clientId, tuple._1, tuple._2)

          out.write(Json.encode[RatioMessage](message).toByteArray)
          out.write("\n".getBytes())
          out.flush()
        })
      }
    }.recoverAndLog()
    ()
  }
}
