package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Lattice}
import replication.authz.AntiEntropy
import replication.sync.ConnectionManager

case class HashDag[T <: HashDagEntry: JsonValueCodec](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, T]
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

  /** Sends the requested entries to `destination`, the counterpart to
    * [[replication.authz.AntiEntropy.sendEventsWithDelta]] without any access control. Since a [[HashDagEntry]]
    * carries its payload inline, shipping the requested entries also ships their payloads, so there is neither a
    * second round of delta value messages, nor any per-payload read permission check (as performed by
    * [[replication.authz.Replica.filterDeltas]] before a delta value may be sent). Entries are sent using the
    * same message encoding as [[replication.authz.AntiEntropy]]'s event messages, so that the resulting message
    * sizes are directly comparable.
    *
    * Unknown hashes are skipped, mirroring [[replication.authz.AntiEntropy.sendEventsWithDelta]].
    */
  def sendEntries[T <: HashDagEntry: JsonValueCodec](
      hashDag: HashDag[T],
      connectionManager: ConnectionManager,
      destination: PublicIdentity,
      entryHashes: Iterable[Hash]
  ): Unit =
    connectionManager.sendMultiple(
      destination,
      entryHashes.flatMap(hashDag.events.get).map(entry => AntiEntropy.encodeEventMsg(writeToArray(entry)))
    )

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

    /** Encodes an entry's payload as base64, the way [[Hash]] and [[Signature]] are encoded, instead of
      * jsoniter's default encoding of an `Array[Byte]` as a JSON array of decimal numbers, which inflates
      * every payload byte to roughly 3.5 bytes on the wire. Without this, the entries of a [[HashDag]] would be
      * several times larger than the equivalent [[replication.authz.ArdtEvent]] plus delta value message, which
      * carries the very same payload as raw bytes, making any comparison between the two a comparison of payload
      * encodings rather than of the cost of access control.
      */
    given payloadValueCodec: JsonValueCodec[Array[Byte]]:
        override def decodeValue(in: JsonReader, default: Array[Byte]): Array[Byte] =
          in.readBase64AsBytes(default)

        override def encodeValue(x: Array[Byte], out: JsonWriter): Unit = out.writeBase64Val(x, true)

        override def nullValue: Array[Byte] = null

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
