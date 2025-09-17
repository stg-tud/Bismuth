package DeltaState

import java.security.{KeyPair, PublicKey, MessageDigest}
import crypto.Ed25519Util

case class GSet[T] (
                     authorKeys: KeyPair = Ed25519Util.generateNewKeyPair,
                     dotStore: Map[T, HashedDots] = Map(),
                     causalContext: Map[PublicKey, HashedDots] = Map()
                   ):

  def elements: Set[T] = dotStore.filter((k, v) => v.dots.size == 1).keySet

  def add(element: T): Delta[T] =
    val oldDot = causalContext.get(authorKeys.getPublic)
    val (counter, hash) = oldDot match
      case Some(dots) => (dots.dots.maxBy(_.counter).counter + 1, dots.dots.maxBy(_.counter).hash)
      case None       => (0, "")

    val newHash = String(MessageDigest.getInstance("SHA-3-512").digest((hash + element.toString).getBytes))
    val signature = Ed25519Util.sign(s"$counter$newHash".getBytes, authorKeys.getPrivate)

    val newDot = HashedDot(
      authorKeys.getPublic,
      counter,
      newHash,
      signature
    )

    Delta(Map(element -> newDot), Map(authorKeys.getPublic -> newDot))

  // def merge(delta: Delta[T]): GSet[T]  = ???



case class Delta[T] (
                     dotStore: Map[T, HashedDot],
                     causalContext: Map[PublicKey, HashedDot]
                   )
