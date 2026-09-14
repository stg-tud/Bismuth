package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Lattice}

case class HashDag[T <: HashDagEntry: JsonValueCodec](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, HashDagEntry]
) {
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], HashDag[T]] = {
    val hash = Hash.compute(encodedEvent)
    if events.contains(hash) then return Right(this)

    val event = readFromArray[T](encodedEvent)
    require(event.isValid)

    if hash != genesis then {
      require(event.parents.nonEmpty)
    } else {
      require(event.parents.isEmpty)
    }

    val missingParents = event.parents.filterNot(events.contains)
    if missingParents.nonEmpty then return Left(missingParents)

    Right(copy(
      heads = (heads -- event.parents) + hash,
      events = events + (hash -> event),
    ))
  }
}

object HashDag {
  /** Inserts an entry into the dag, throwing if it is invalid or references unknown dependencies. */
  def receiveOrThrow[T <: HashDagEntry](hashDag: HashDag[T], encodedEntry: Array[Byte]): HashDag[T] =
    hashDag.receive(encodedEntry) match {
      case Right(updated) => updated
      case Left(missing)  =>
        throw new IllegalStateException(s"Entry is missing dependencies: $missing")
    }

  /** Full state materialization from every entry's payload, without any access control enforcement (unlike
    * [[replication.authz.Authorization.materialize]]).
    */
  def materialize[R: {Lattice, Bottom, JsonValueCodec}](hashDag: HashDag[?]): R =
    hashDag.events.values.iterator.foldLeft(Bottom[R].empty) { (acc, entry) =>
      acc.merge(readFromArray[R](entry.payload))
    }
}

trait HashDagEntry:
    def payload: Array[Byte]
    def author: PublicIdentity
    def parents: Set[Hash]
    def hash: Hash
    def isValid: Boolean

case class SignedHashDagEntry(
    payload: Array[Byte],
    author: PublicIdentity,
    parents: Set[Hash],
    signature: Signature
) extends HashDagEntry:
    override def hash: Hash       = Hash.compute(writeToArray(this))
    override def isValid: Boolean =
      signature.verify(author.publicKey, writeToArray(copy(signature = Signature.allZeroSignature)))

case class UnsignedHashDagEntry(
    payload: Array[Byte],
    author: PublicIdentity,
    parents: Set[Hash],
) extends HashDagEntry:
    override def hash: Hash       = Hash.compute(writeToArray(this))
    override def isValid: Boolean = true

object HashDagEntry:
    import replication.JsoniterCodecsJvm.given
    given JsonValueCodec[SignedHashDagEntry]   = JsonCodecMaker.make
    given JsonValueCodec[UnsignedHashDagEntry] = JsonCodecMaker.make

    def createSignedEntry[P: JsonValueCodec](
        payload: P,
        privateIdentity: PrivateIdentity,
        parents: Set[Hash]
    ): SignedHashDagEntry = {
      val unsignedEntry = SignedHashDagEntry(
        writeToArray(payload),
        privateIdentity.getPublic,
        parents,
        Signature.allZeroSignature,
      )
      val sk        = privateIdentity.identityKey.getPrivate
      val signature = Signature.compute(writeToArray(unsignedEntry), sk)
      unsignedEntry.copy(signature = signature)
    }

    def createUnsignedEntry[P: JsonValueCodec](
        payload: P,
        privateIdentity: PrivateIdentity,
        parents: Set[Hash]
    ): UnsignedHashDagEntry = UnsignedHashDagEntry(
      writeToArray(payload),
      privateIdentity.getPublic,
      parents,
    )
