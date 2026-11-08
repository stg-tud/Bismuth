package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import channels.crypto.PrivateIdentity
import channels.crypto.PublicIdentity
import channels.crypto.Hash
import rdts.base.{Bottom, Decompose, Lattice}
import channels.connection.ByteBufferMessageBuffer
import channels.broadcast.authz.AuthzMessage
import ex2026accessControl.acl.sync.ConnectionManager

class HashDagReplica[Entry <: HashDagEntry[RDT]: JsonValueCodec, RDT: {Lattice, Bottom, JsonValueCodec, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    createEntry: (RDT, PrivateIdentity, Set[Hash]) => Entry,
    connectionManager: => ConnectionManager
) {

  @volatile var hashDag: HashDag[RDT, Entry] = HashDag(genesis, Set.empty, Map.empty)
  @volatile var materializedState: RDT       = Bottom[RDT].empty

  def heads: Set[Hash] = hashDag.heads

  def receiveEntry(encodedEntry: Array[Byte]): Unit = synchronized {
    val oldHeads = hashDag.heads
    hashDag = HashDag.receiveOrThrow(hashDag, encodedEntry)
    hashDag.heads.diff(oldHeads).headOption match {
      case Some(addedEntry) =>
        materializedState = materializedState.merge(hashDag.events(addedEntry).payload)
      case None => ??? // There shouldn't be any duplicates in the benchmark
    }
  }

  /** Like [[channels.broadcast.authz.AuthzReplica.write]], but without any access control: every decomposed delta
    * becomes its own entry, each one on top of the previous.
    */
  def mutateState(mutator: RDT => RDT): Unit = synchronized {
    val delta = mutator(materializedState)

    // Apply locally
    var dag                                           = hashDag
    var updatedState                                  = materializedState
    val entries: Iterable[(Hash, Entry, Array[Byte])] =
      Decompose.decompose(delta).map { decomposedDelta =>
        val entry        = createEntry(decomposedDelta, privateIdentity, dag.heads)
        val encodedEntry = writeToArray(entry)
        val hash         = Hash.compute(encodedEntry)
        dag = dag.copy(heads = Set(hash), events = dag.events + (hash -> entry))
        updatedState = updatedState.merge(decomposedDelta)
        (hash, entry, encodedEntry)
      }.toList
    hashDag = dag
    materializedState = updatedState

    disseminate(entries)
  }

  protected def disseminate(entries: Iterable[(Hash, Entry, Array[Byte])]): Unit =
    connectionManager.broadcast(List(HashDagReplica.encodeEntries(entries.map(_._3))))

  def sendEntries(destination: PublicIdentity, entryHashes: Iterable[Hash]): Unit =
    connectionManager.sendMultiple(
      destination,
      List(HashDagReplica.encodeEntries(entryHashes.flatMap(hashDag.events.get).map(entry => writeToArray(entry))))
    )
}

object HashDagReplica {

  /** Entries in the same wire format as the events of [[channels.broadcast.authz.AuthzProtocol]]. */
  def encodeEntries(entries: Iterable[Array[Byte]]): ByteBufferMessageBuffer =
    ByteBufferMessageBuffer(writeToArray[AuthzMessage](AuthzMessage.Events(entries.toList))(using AuthzMessage.codec))
}
