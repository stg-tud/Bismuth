package benchmarks.crypto

import crypto.Ed25519Util
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.security.*
import java.util.concurrent.TimeUnit
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime, Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(2)
@Warmup(iterations = 1, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class HashAndSignatureBench {
  // (Quick and dirty) Macbook Pro M4 Pro Results:
  // Benchmark  (messageLengthBytes)  (provider) Mode   Cnt      Score      Error   Units
  // checkSignature         100          BC      thrpt    6     26.783 ±    0.184  ops/ms
  // checkSignature         100         SUN      thrpt    6      3.727 ±    0.055  ops/ms
  // checkSignature        1000          BC      thrpt    6     26.414 ±    0.341  ops/ms
  // checkSignature        1000         SUN      thrpt    6      3.683 ±    0.081  ops/ms
  // checkSignature       10000          BC      thrpt    6     17.467 ±    0.206  ops/ms
  // checkSignature       10000         SUN      thrpt    6      3.652 ±    0.192  ops/ms
  // sha256                 100          BC      thrpt    6   2628.354 ±   63.217  ops/ms
  // sha256                 100         SUN      thrpt    6  24067.045 ± 1580.417  ops/ms
  // sha256                1000          BC      thrpt    6    341.661 ±    3.157  ops/ms
  // sha256                1000         SUN      thrpt    6   3309.901 ±   75.397  ops/ms
  // sha256               10000          BC      thrpt    6     35.162 ±    2.022  ops/ms
  // sha256               10000         SUN      thrpt    6    333.428 ±    2.877  ops/ms
  // sha3_256               100          BC      thrpt    6   5132.435 ±  201.143  ops/ms
  // sha3_256               100         SUN      thrpt    6   7444.119 ±  124.148  ops/ms
  // sha3_256              1000          BC      thrpt    6    732.071 ±    2.115  ops/ms
  // sha3_256              1000         SUN      thrpt    6   1001.533 ±   12.277  ops/ms
  // sha3_256             10000          BC      thrpt    6     80.750 ±    0.223  ops/ms
  // sha3_256             10000         SUN      thrpt    6    103.579 ±    8.189  ops/ms
  // sha3_512               100          BC      thrpt    6   2865.309 ±   35.427  ops/ms
  // sha3_512               100         SUN      thrpt    6   3848.671 ±   52.314  ops/ms
  // sha3_512              1000          BC      thrpt    6    437.233 ±   13.757  ops/ms
  // sha3_512              1000         SUN      thrpt    6    557.900 ±    3.176  ops/ms
  // sha3_512             10000          BC      thrpt    6     44.714 ±    1.400  ops/ms
  // sha3_512             10000         SUN      thrpt    6     56.195 ±    0.460  ops/ms
  // sha512                 100          BC      thrpt    6   3684.349 ±   78.377  ops/ms
  // sha512                 100         SUN      thrpt    6  11243.713 ±  546.921  ops/ms
  // sha512                1000          BC      thrpt    6    496.895 ±   65.508  ops/ms
  // sha512                1000         SUN      thrpt    6   1841.555 ±   25.859  ops/ms
  // sha512               10000          BC      thrpt    6     50.710 ±    7.563  ops/ms
  // sha512               10000         SUN      thrpt    6    185.875 ±    3.308  ops/ms
  // sign                   100          BC      thrpt    6      3.749 ±    0.014  ops/ms
  // sign                   100         SUN      thrpt    6      3.757 ±    0.033  ops/ms
  // sign                  1000          BC      thrpt    6      3.733 ±    0.017  ops/ms
  // sign                  1000         SUN      thrpt    6      3.774 ±    0.145  ops/ms
  // sign                 10000          BC      thrpt    6      3.655 ±    0.044  ops/ms
  // sign                 10000         SUN      thrpt    6      3.648 ±    0.068  ops/ms
  // checkSignature         100          BC       avgt    6      0.038 ±    0.002   ms/op
  // checkSignature         100         SUN       avgt    6      0.263 ±    0.003   ms/op
  // checkSignature        1000          BC       avgt    6      0.040 ±    0.001   ms/op
  // checkSignature        1000         SUN       avgt    6      0.270 ±    0.010   ms/op
  // checkSignature       10000          BC       avgt    6      0.058 ±    0.001   ms/op
  // checkSignature       10000         SUN       avgt    6      0.275 ±    0.026   ms/op
  // sha256                 100          BC       avgt    6     ≈ 10⁻³              ms/op
  // sha256                 100         SUN       avgt    6     ≈ 10⁻⁴              ms/op
  // sha256                1000          BC       avgt    6      0.003 ±    0.001   ms/op
  // sha256                1000         SUN       avgt    6     ≈ 10⁻⁴              ms/op
  // sha256               10000          BC       avgt    6      0.029 ±    0.003   ms/op
  // sha256               10000         SUN       avgt    6      0.003 ±    0.001   ms/op
  // sha3_256               100          BC       avgt    6     ≈ 10⁻⁴              ms/op
  // sha3_256               100         SUN       avgt    6     ≈ 10⁻⁴              ms/op
  // sha3_256              1000          BC       avgt    6      0.001 ±    0.001   ms/op
  // sha3_256              1000         SUN       avgt    6      0.001 ±    0.001   ms/op
  // sha3_256             10000          BC       avgt    6      0.013 ±    0.001   ms/op
  // sha3_256             10000         SUN       avgt    6      0.009 ±    0.001   ms/op
  // sha3_512               100          BC       avgt    6     ≈ 10⁻³              ms/op
  // sha3_512               100         SUN       avgt    6     ≈ 10⁻⁴              ms/op
  // sha3_512              1000          BC       avgt    6      0.002 ±    0.001   ms/op
  // sha3_512              1000         SUN       avgt    6      0.002 ±    0.001   ms/op
  // sha3_512             10000          BC       avgt    6      0.022 ±    0.001   ms/op
  // sha3_512             10000         SUN       avgt    6      0.017 ±    0.001   ms/op
  // sha512                 100          BC       avgt    6     ≈ 10⁻⁴              ms/op
  // sha512                 100         SUN       avgt    6     ≈ 10⁻⁴              ms/op
  // sha512                1000          BC       avgt    6      0.002 ±    0.001   ms/op
  // sha512                1000         SUN       avgt    6      0.001 ±    0.001   ms/op
  // sha512               10000          BC       avgt    6      0.019 ±    0.003   ms/op
  // sha512               10000         SUN       avgt    6      0.005 ±    0.001   ms/op
  // sign                   100          BC       avgt    6      0.264 ±    0.001   ms/op
  // sign                   100         SUN       avgt    6      0.261 ±    0.010   ms/op
  // sign                  1000          BC       avgt    6      0.260 ±    0.005   ms/op
  // sign                  1000         SUN       avgt    6      0.265 ±    0.002   ms/op
  // sign                 10000          BC       avgt    6      0.273 ±    0.010   ms/op
  // sign                 10000         SUN       avgt    6      0.290 ±    0.065   ms/op
  @Param(Array("100", "1000", "10000"))
  var messageLengthBytes: Int = scala.compiletime.uninitialized
  @Param(Array("BC", "SUN"))
  var provider: String = scala.compiletime.uninitialized

  var privateKey: PrivateKey     = scala.compiletime.uninitialized
  var publicKey: PublicKey       = scala.compiletime.uninitialized
  var testData: Array[Byte]      = scala.compiletime.uninitialized
  var testSignature: Array[Byte] = scala.compiletime.uninitialized
  var sha256: MessageDigest      = scala.compiletime.uninitialized
  var sha512: MessageDigest      = scala.compiletime.uninitialized
  var sha3_256: MessageDigest    = scala.compiletime.uninitialized
  var sha3_512: MessageDigest    = scala.compiletime.uninitialized
  var ed25519: Signature         = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    val keypair = Ed25519Util.generateNewKeyPair
    privateKey = keypair.getPrivate
    publicKey = keypair.getPublic
    testData = Random(42).nextBytes(messageLengthBytes)
    testSignature = Ed25519Util.sign(testData, privateKey)

    sha256 = MessageDigest.getInstance("SHA-256", provider)
    sha512 = MessageDigest.getInstance("SHA-512", provider)
    sha3_256 = MessageDigest.getInstance("SHA3-256", provider)
    sha3_512 = MessageDigest.getInstance("SHA3-512", provider)
    ed25519 = Signature.getInstance("ED25519", if provider == "SUN" then "SunEC" else provider)
  }

  @Benchmark
  def checkSignature(blackhole: Blackhole): Unit =
      ed25519.initVerify(publicKey)
      ed25519.update(testData)
      blackhole.consume(ed25519.verify(testSignature))

  @Benchmark
  def sign(blackhole: Blackhole): Unit =
    blackhole.consume(Ed25519Util.sign(testData, privateKey))

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
