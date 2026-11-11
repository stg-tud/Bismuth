package channels.experiments

import java.io.{FileInputStream, IOException}

/** Platform source of cryptographically secure random bytes (Scala Native has no `java.security.SecureRandom`). */
object SecureRandomBytes {
  def fill(bytes: Array[Byte]): Unit =
    val in = new FileInputStream("/dev/urandom")
    try
      var off = 0
      while off < bytes.length do
        val n = in.read(bytes, off, bytes.length - off)
        if n < 0 then throw IOException("/dev/urandom closed unexpectedly")
        off += n
    finally in.close()
}
