package channels.crypto

import channels.Platform

import java.io.{FileInputStream, IOException}
import java.security.SecureRandom

/** Platform source of cryptographically secure random bytes. Scala Native has no `java.security.SecureRandom` and reads
  * `/dev/urandom` instead.
  */
object SecureRandomBytes {
  private lazy val random = new SecureRandom()

  def fill(bytes: Array[Byte]): Unit =
    if Platform.isNative then fillFromUrandom(bytes)
    else random.nextBytes(bytes)

  private def fillFromUrandom(bytes: Array[Byte]): Unit =
      val in = new FileInputStream("/dev/urandom")
      try
          var off = 0
          while off < bytes.length do
              val n = in.read(bytes, off, bytes.length - off)
              if n < 0 then throw IOException("/dev/urandom closed unexpectedly")
              off += n
      finally in.close()
}
