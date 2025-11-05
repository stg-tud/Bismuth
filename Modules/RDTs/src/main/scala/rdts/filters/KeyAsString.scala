package rdts.filters

trait KeyAsString[K]:
   def encode(key: K): String

   def decode(string: String): K

object KeyAsString {
  inline def apply[K](using keyCodec: KeyAsString[K]): KeyAsString[K] = keyCodec

  given stringCoder: KeyAsString[String]:
     override def encode(key: String): String = key

     override def decode(string: String): String = string
}
