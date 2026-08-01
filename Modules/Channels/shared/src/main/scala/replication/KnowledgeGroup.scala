package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}

case class KnowledgeGroup[A](ids: Set[Uid], path: A => A, enabled: A => Boolean) {
  def matches(delta: A)(using Bottom[A]): Boolean = {
    val pathDelta = path(delta)
    enabled(delta) && !Bottom[A].isEmpty(pathDelta)
  }

//  def setupConnections(handleDelta: A => Unit)(using JsonValueCodec[A], Lattice[ProtocolMessage.Payload[A]]) = {
//    val idList @ primary :: secondaries = ids.toList
//    val dataManagers                    = idList.map(id =>
//      (
//        id,
//        DeltaDissemination(
//          id,
//          delta => handleDelta(delta),
//          defaultTimetolive = 0,
//          deltaStorage = DeltaStorage.getStorage(DeltaStorage.Type.KeepAll, () => ???)
//        )
//      )
//    ).toMap
//    val connection = channels.SynchronousLocalConnection[ProtocolMessage[A]]()
//  }
}

case class PrdtSystem[A](knowledgeGroups: Set[KnowledgeGroup[A]]) {
  def groupsFor(uid: Uid) =
    knowledgeGroups.filter(_.ids.contains(uid))

  def matches(delta: A, receiver: Uid)(using Bottom[A], LocalUid) =
    knowledgeGroups.exists(g => g.ids.contains(replicaId) && g.ids.contains(receiver) && g.matches(delta))

  def matches(delta: A, receivers: Set[Uid])(using Bottom[A], LocalUid) =
    knowledgeGroups.exists(g =>  g.ids.contains(replicaId) && g.ids == receivers && g.matches(delta))

}
