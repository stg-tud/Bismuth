package channels.experiments

import java.security.SecureRandom

/** Platform source of cryptographically secure random bytes. */
object SecureRandomBytes {
  private lazy val random = new SecureRandom()

  def fill(bytes: Array[Byte]): Unit = random.nextBytes(bytes)
}
