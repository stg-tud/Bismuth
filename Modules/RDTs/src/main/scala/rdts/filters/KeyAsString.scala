package rdts.filters

trait KeyAsString[K]:
    def encode(key: K): String

    def decode(string: String): K

object KeyAsString {
  inline def apply[K](using keyCodec: KeyAsString[K]): KeyAsString[K] = keyCodec

  given stringCoder: KeyAsString[String]:
      override def encode(key: String): String = key

      override def decode(string: String): String = string

  /** Encodes dots as `uid:time`. */
  given dotCoder: KeyAsString[rdts.time.Dot]:
      override def encode(key: rdts.time.Dot): String = s"${key.place.delegate}:${key.time}"

      override def decode(string: String): rdts.time.Dot =
          val sep = string.lastIndexOf(':')
          rdts.time.Dot(rdts.base.Uid(string.substring(0, sep)), string.substring(sep + 1).toLong)
}
