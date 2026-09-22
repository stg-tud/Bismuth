package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Lattice}
import replication.authz.AntiEntropy
import replication.sync.ConnectionManager

class HashDagReplica[Entry <: HashDagEntry: JsonValueCodec, RDT: {Lattice, Bottom, JsonValueCodec}](
    genesis: Hash,
    connectionManager: => ConnectionManager
) {

  @volatile private var hashDag: HashDag[Entry] = HashDag(genesis, Set.empty, Map.empty)
  @volatile private var materializedState: RDT  = Bottom[RDT].empty

  def heads: Set[Hash] = hashDag.heads

  def receiveEntry(encodedEntry: Array[Byte]): Unit = synchronized {
    val oldHeads = hashDag.heads
    hashDag = HashDag.receiveOrThrow(hashDag, encodedEntry)
    val addedEntry = hashDag.heads.diff(oldHeads).head
    materializedState = materializedState.merge(readFromArray[RDT](hashDag.events(addedEntry).payload))
  }

  def sendEntries(destination: PublicIdentity, entryHashes: Iterable[Hash]): Unit =
    connectionManager.sendMultiple(
      destination,
      entryHashes.flatMap(hashDag.events.get).map(entry => AntiEntropy.encodeEventMsg(writeToArray(entry)))
    )
}
