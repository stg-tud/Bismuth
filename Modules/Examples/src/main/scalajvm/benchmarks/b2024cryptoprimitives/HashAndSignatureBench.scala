package benchmarks.b2024cryptoprimitives

import channels.crypto.Ed25519Util
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import pt.kcry.blake3.Blake3

import java.security.*
import java.util.concurrent.TimeUnit
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(2)
@Warmup(iterations = 1, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
class SignatureBench {
  @Param(Array("32", "100", "1000", "10000", "100000"))
  var messageLengthBytes: Int = scala.compiletime.uninitialized
  @Param(Array("BC", "SunEC"))
  var provider: String = scala.compiletime.uninitialized

  var privateKey: PrivateKey     = scala.compiletime.uninitialized
  var publicKey: PublicKey       = scala.compiletime.uninitialized
  var testData: Array[Byte]      = scala.compiletime.uninitialized
  var testSignature: Array[Byte] = scala.compiletime.uninitialized

  var ed25519: Signature = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    if Security.getProvider("BC") == null then Security.addProvider(BouncyCastleProvider()): Unit
    val keypair = Ed25519Util.generateNewKeyPair
    privateKey = keypair.getPrivate
    publicKey = keypair.getPublic
    testData = Random(42).nextBytes(messageLengthBytes)
    testSignature = Ed25519Util.sign(testData, privateKey)

    ed25519 = Signature.getInstance("Ed25519", provider)
  }

  @Benchmark
  def checkSignature(blackhole: Blackhole): Unit =
      ed25519.initVerify(publicKey)
      ed25519.update(testData)
      blackhole.consume(ed25519.verify(testSignature))

  @Benchmark
  def sign(blackhole: Blackhole): Unit =
      ed25519.initSign(privateKey)
      ed25519.update(testData)
      blackhole.consume(ed25519.sign())
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(2)
@Warmup(iterations = 1, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
class HashBench {
  @Param(Array("32", "100", "1000", "10000", "100000"))
  var messageLengthBytes: Int = scala.compiletime.uninitialized
  @Param(Array("BC", "SUN"))
  var provider: String = scala.compiletime.uninitialized

  var testData: Array[Byte] = scala.compiletime.uninitialized

  var sha256: MessageDigest   = scala.compiletime.uninitialized
  var sha512: MessageDigest   = scala.compiletime.uninitialized
  var sha3_256: MessageDigest = scala.compiletime.uninitialized
  var sha3_512: MessageDigest = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    if Security.getProvider("BC") == null then Security.addProvider(BouncyCastleProvider()): Unit
    testData = Random(42).nextBytes(messageLengthBytes)

    sha256 = MessageDigest.getInstance("SHA-256", provider)
    sha512 = MessageDigest.getInstance("SHA-512", provider)
    sha3_256 = MessageDigest.getInstance("SHA3-256", provider)
    sha3_512 = MessageDigest.getInstance("SHA3-512", provider)
  }

  @Benchmark
  def sha256(blackhole: Blackhole): Unit =
    blackhole.consume(sha256.digest(testData))

  @Benchmark
  def sha512(blackhole: Blackhole): Unit =
    blackhole.consume(sha512.digest(testData))

  @Benchmark
  def sha3_256(blackhole: Blackhole): Unit =
    blackhole.consume(sha3_256.digest(testData))

  @Benchmark
  def sha3_512(blackhole: Blackhole): Unit =
    blackhole.consume(sha3_512.digest(testData))
}

/** BLAKE3 is not available in the SUN provider, so it is benchmarked separately with BouncyCastle only. */
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(2)
@Warmup(iterations = 1, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
class Blake3Bench {
  @Param(Array("32", "100", "1000", "10000", "100000"))
  var messageLengthBytes: Int = scala.compiletime.uninitialized

  var testData: Array[Byte]     = scala.compiletime.uninitialized
  var blake3_256: MessageDigest = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    if Security.getProvider("BC") == null then Security.addProvider(BouncyCastleProvider()): Unit
    testData = Random(42).nextBytes(messageLengthBytes)
    blake3_256 = MessageDigest.getInstance("BLAKE3-256", "BC")
  }

  @Benchmark
  def blake3_256_bc(blackhole: Blackhole): Unit =
    blackhole.consume(blake3_256.digest(testData))

  @Benchmark
  def blake3_256_pure_scala(blackhole: Blackhole): Unit =
    blackhole.consume(Blake3.hash(testData))
}
