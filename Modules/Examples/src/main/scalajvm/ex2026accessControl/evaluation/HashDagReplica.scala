package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Decompose, Lattice}
import replication.authz.AntiEntropy
import replication.sync.ConnectionManager

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

  /** Like [[replication.authz.Replica.mutateState]], but without any access control: every decomposed delta
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
    connectionManager.broadcast(entries.map(entry => AntiEntropy.encodeEventMsg(entry._3)))

  def sendEntries(destination: PublicIdentity, entryHashes: Iterable[Hash]): Unit =
    connectionManager.sendMultiple(
      destination,
      entryHashes.flatMap(hashDag.events.get).map(entry => AntiEntropy.encodeEventMsg(writeToArray(entry)))
    )
}
