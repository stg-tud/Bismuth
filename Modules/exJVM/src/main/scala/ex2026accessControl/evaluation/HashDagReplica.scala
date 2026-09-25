package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Lattice}
import replication.authz.AntiEntropy
import replication.sync.ConnectionManager

class HashDagReplica[Entry <: HashDagEntry[RDT]: JsonValueCodec, RDT: {Lattice, Bottom, JsonValueCodec}](
    genesis: Hash,
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

  def sendEntries(destination: PublicIdentity, entryHashes: Iterable[Hash]): Unit =
    connectionManager.sendMultiple(
      destination,
      entryHashes.flatMap(hashDag.events.get).map(entry => AntiEntropy.encodeEventMsg(writeToArray(entry)))
    )
}
