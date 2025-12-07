package dag

//import com.github.plokhotnyuk.jsoniter_scala.core.*
//import com.github.plokhotnyuk.jsoniter_scala.macros.*

import crypto.Ed25519Util
import java.security.{MessageDigest, PublicKey}
import java.util.Base64

case class Event[T](
    id: String,
    content: Option[T],
    author: PublicKey,
    dependencies: Set[String],
    signature: Array[Byte],
    // authorIsByzantine: Boolean = false
):

    private val HashAlgorithm = "SHA3-512"

    def calculateHash: String =
        val bytes =
          s"${content.toString}${author.toString}${dependencies.toString}${Base64.getEncoder.encodeToString(signature)}"
            .getBytes


        Base64.getEncoder.encodeToString(MessageDigest.getInstance(HashAlgorithm).digest(bytes))

    def signatureIsValid: Boolean =
      Ed25519Util.checkEd25519Signature(content.get.toString.getBytes, signature, author)

object Event:

    def apply[T](content: Option[T], author: PublicKey, dependencies: Set[String], signature: Array[Byte]): Event[T] =
        val tmp = Event("", content, author, dependencies, signature)
        val id  = tmp.calculateHash

        Event(id, content, author, dependencies, signature)
