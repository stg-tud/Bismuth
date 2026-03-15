package lofi_acl.evaluation

import cats.data.{Validated, ValidatedNel}
import cats.implicits.*
import com.monovore.decline.*
import crypto.channels.IdentityFactory
import lofi_acl.evaluation.PingPongBenchmarkRunnerArguments.Mode.*
import lofi_acl.evaluation.PingPongBenchmarkRunnerArguments.{Config, Peer}

import java.nio.file.{Path, Paths}
import scala.language.implicitConversions

object PingPongBenchmarkRunner extends CommandApp(
      name = "pingpong",
      header = "Ping pong benchmark runner",
      main = PingPongBenchmarkRunnerArguments.configOpts.map { config =>
        given scala.Conversion[Seq[Peer], Seq[(String, Int)]] = peers => peers.map(peer => (peer.host, peer.port))

        val savedTrace = CreateAndSaveTrace.load(config.trace)
        val trace      = Trace(
          savedTrace.identities.map(IdentityFactory.fromIdentityKey),
          savedTrace.aclGenesis,
          savedTrace.additionalPermissions,
          savedTrace.deltas
        )
        config match
            case Config(mode, replica, host, port, peers, numPeersExpected, enforceAcl, _, iterations, out) =>
              val instance = PingPongBenchmark(host, port, peers, trace, enforceAcl)
              mode match {
                case Relay    => instance.runAsRelay(savedTrace.relayPrivateIdentity, iterations)
                case Leader   => instance.runAsLeader(iterations, numPeersExpected.get, out)
                case Follower => instance.runAsFollower(replica.get, iterations)
              }
      }
    )

object PingPongBenchmarkRunnerArguments {
  enum Mode:
      case Relay, Leader, Follower

  case class Peer(host: String, port: Int)

  case class Config(
      mode: Mode,
      replica: Option[Int],
      host: String,
      port: Int,
      peers: List[Peer],
      expectedPeers: Option[Int],
      enforce: Boolean,
      trace: Path,
      iterations: Int,
      outFile: Option[Path]
  )

  given Argument[Mode] with
      def read(string: String): ValidatedNel[String, Mode] = string.toLowerCase match
          case "relay"    => Validated.valid(Relay)
          case "leader"   => Validated.valid(Mode.Leader)
          case "follower" => Validated.valid(Mode.Follower)
          case other      => Validated.invalidNel(s"Unknown mode '$other'. Expected: relay, leader, or follower.")

      def defaultMetavar: String = "mode"

  given Argument[Peer] with
      def read(string: String): ValidatedNel[String, Peer] =
        string.split(":", 2) match
            case Array(h, p) =>
              p.toIntOption
                .filter(_ > 0)
                .toRight(s"Peer port must be a positive integer, got: '$p'")
                .fold(Validated.invalidNel, p => Validated.valid(Peer(h, p)))
            case _ => Validated.invalidNel(s"Peer must be in 'host:port' format, got: '$string'")

      def defaultMetavar: String = "host:port"

  val modeOpt: Opts[Mode] =
    Opts.option[Mode]("mode", help = "One of: relay, leader, follower.")

  val replicaOpt: Opts[Option[Int]] =
    Opts.option[Int]("replica", help = "Replica number.")
      .orNone

  val hostOpt: Opts[String] =
    Opts.option[String]("host", help = "Hostname to bind to.").withDefault("localhost")

  val portOpt: Opts[Int] =
    Opts.option[Int]("port", help = "Port number (positive integer).")
      .mapValidated(p => cats.data.Validated.condNel(p >= 0, p, "Port must be non-negative."))
      .withDefault(0)

  val peerOpts: Opts[List[Peer]] =
    Opts.options[Peer]("peer", help = "A peer in host:port format. May be repeated.").orEmpty

  val expectedPeersOpt: Opts[Option[Int]] =
    Opts.option[Int]("expected-peers", help = "Number of peers the replica expects.")
      .mapValidated(p => cats.data.Validated.condNel(p >= 0, p, "expected-peers must be non-negative."))
      .orNone

  val enforceOpt: Opts[Boolean] =
    Opts.flag("enforce", help = "Enforce the ACL.").as(true)
      .orElse(Opts.flag("no-enforce", "Don't enforce the ACL").as(false))
      .withDefault(true)

  val traceOpt: Opts[Path] =
    Opts.option[Path]("trace", help = "Path to the trace to read from.")
      .withDefault(Paths.get("ping-pong-trace.json"))

  val iterationsOpt: Opts[Int] =
    Opts.option[Int]("iterations", help = "Number of iterations.")
      .mapValidated(n => cats.data.Validated.condNel(n > 0, n, "Iterations must be a positive integer."))

  val outOpt: Opts[Option[Path]] =
    Opts.option[Path]("out", help = "Path to the results file.")
      .orNone

  val configOpts: Opts[Config] =
    (modeOpt, replicaOpt, hostOpt, portOpt, peerOpts, expectedPeersOpt, enforceOpt, traceOpt, iterationsOpt, outOpt)
      .mapN {
        (modeOpt, replicaOpt, hostOpt, portOpt, peerOpts, expectedPeersOpt, enforceOpt, traceOpt, iterationsOpt, out) =>
          if out.nonEmpty && modeOpt != Leader then Validated.invalidNel("--out must only be used with --mode=leader")
          else
              (modeOpt, expectedPeersOpt, replicaOpt) match {
                case (Relay | Leader, _, Some(_)) =>
                  Validated.invalidNel("--replica must not be set when --mode is relay or leader")
                case (Follower, _, None) =>
                  Validated.invalidNel("--replica must be set when --mode is follower")
                case (Relay | Follower, Some(_), _) =>
                  Validated.invalidNel("--expected-peers must only be used when --mode is leader")
                case (Leader, None, _) =>
                  Validated.invalidNel("--expected-peers must be set when --mode is leader")
                case _ =>
                  Validated.valid(
                    Config(
                      modeOpt,
                      replicaOpt,
                      hostOpt,
                      portOpt,
                      peerOpts,
                      expectedPeersOpt,
                      enforceOpt,
                      traceOpt,
                      iterationsOpt,
                      out
                    )
                  )
              }
      }.mapValidated(identity)
}
