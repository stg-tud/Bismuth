package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Lattice}

case class HashDag[P: JsonValueCodec, T <: HashDagEntry[P]: JsonValueCodec](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, T]
) {
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], HashDag[P, T]] = {
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
  def receiveOrThrow[P, T <: HashDagEntry[P]](hashDag: HashDag[P, T], encodedEntry: Array[Byte]): HashDag[P, T] =
    hashDag.receive(encodedEntry) match {
      case Right(updated) => updated
      case Left(missing)  =>
        throw new IllegalStateException(s"Entry is missing dependencies: $missing")
    }

  /** Full state materialization from every entry's payload, without any access control enforcement (unlike
    * [[replication.authz.Authorization.materialize]]).
    */
  def materialize[R: {Lattice, Bottom}](hashDag: HashDag[R, ?]): R =
    hashDag.events.values.iterator.foldLeft(Bottom[R].empty) { (acc, entry) =>
      acc.merge(entry.payload)
    }
}

sealed trait HashDagEntry[P]:
    def payload: P
    def author: PublicIdentity
    def parents: Set[Hash]
    def hash(using JsonValueCodec[P]): Hash
    def isValid(using JsonValueCodec[P]): Boolean

case class SignedHashDagEntry[P](
    payload: P,
    author: PublicIdentity,
    parents: Set[Hash],
    signature: Signature
) extends HashDagEntry[P]:
    override def hash(using JsonValueCodec[P]): Hash       = Hash.compute(writeToArray(this))
    override def isValid(using JsonValueCodec[P]): Boolean =
      signature.verify(author.publicKey, writeToArray(copy(signature = Signature.allZeroSignature)))

case class UnsignedHashDagEntry[P](
    payload: P,
    author: PublicIdentity,
    parents: Set[Hash],
) extends HashDagEntry[P]:
    override def hash(using JsonValueCodec[P]): Hash       = Hash.compute(writeToArray(this))
    override def isValid(using JsonValueCodec[P]): Boolean = true

object HashDagEntry:
    import replication.JsoniterCodecsJvm.given

    given signedHashDagEntryCodec[P: JsonValueCodec]: JsonValueCodec[SignedHashDagEntry[P]]     = JsonCodecMaker.make
    given unsignedHashDagEntryCodec[P: JsonValueCodec]: JsonValueCodec[UnsignedHashDagEntry[P]] = JsonCodecMaker.make

    def createSignedEntry[P: JsonValueCodec](
        payload: P,
        privateIdentity: PrivateIdentity,
        parents: Set[Hash]
    ): SignedHashDagEntry[P] = {
      val unsignedEntry = SignedHashDagEntry(
        payload,
        privateIdentity.getPublic,
        parents,
        Signature.allZeroSignature,
      )
      val sk        = privateIdentity.identityKey.getPrivate
      val signature = Signature.compute(writeToArray(unsignedEntry), sk)
      unsignedEntry.copy(signature = signature)
    }

    def createUnsignedEntry[P](
        payload: P,
        privateIdentity: PrivateIdentity,
        parents: Set[Hash]
    ): UnsignedHashDagEntry[P] = UnsignedHashDagEntry(
      payload,
      privateIdentity.getPublic,
      parents,
    )
