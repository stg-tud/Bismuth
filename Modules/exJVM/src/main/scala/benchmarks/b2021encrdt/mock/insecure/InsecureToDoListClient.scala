package benchmarks.b2021encrdt.mock.insecure

import benchmarks.b2021encrdt.deltabased.{DecryptedDeltaGroup, EncryptedDeltaGroup}
import benchmarks.b2021encrdt.mock.SecureToDoListClient.ToDoMapLattice
import benchmarks.b2021encrdt.mock.{SecureToDoListClient, ToDoListIntermediary}
import benchmarks.b2021encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import rdts.base.LocalUid
import rdts.syntax.oldCompat.DeltaAWLWWMContainer

import java.util.UUID

class InsecureToDoListClient(
    replicaId1: LocalUid,
    crdt: DeltaAWLWWMContainer[UUID, ToDoEntry],
    untrustedReplica: ToDoListIntermediary
) extends SecureToDoListClient(replicaId1, crdt, null, untrustedReplica) {
  override protected def encryptAndDisseminate(newDeltaGroup: DecryptedDeltaGroup[ToDoMapLattice]): Unit = {
    // Serialize but don't encrypt!
    val serialPlaintextDeltaGroup = writeToArray(newDeltaGroup.deltaGroup)
    val serialDottedVersionVector = writeToArray(newDeltaGroup.dottedVersionVector)

    disseminate(
      EncryptedDeltaGroup(serialPlaintextDeltaGroup, serialDottedVersionVector)
    )
  }
}
