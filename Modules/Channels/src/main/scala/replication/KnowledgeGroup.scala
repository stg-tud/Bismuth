package replication

import rdts.base.{Bottom, ReplicaId, Uid}

case class KnowledgeGroup[A](ids: Set[Uid], path: A => A, enabled: A => Boolean) {
  def matches(delta: A)(using Bottom[A]): Boolean = {
    val pathDelta = path(delta)
    enabled(delta) && !Bottom[A].isEmpty(pathDelta)
  }
}

case class PrdtSystem[A](knowledgeGroups: Set[KnowledgeGroup[A]]) {
  def groupsFor(uid: Uid) =
    knowledgeGroups.filter(_.ids.contains(uid))

  def matches(delta: A, receiver: Uid)(using Bottom[A], ReplicaId) =
    val localReplicaId = summon[ReplicaId].uid
    knowledgeGroups.exists(g => g.ids.contains(localReplicaId) && g.ids.contains(receiver) && g.matches(delta))

  def matches(delta: A, receivers: Set[Uid])(using Bottom[A], ReplicaId) =
    val localReplicaId = summon[ReplicaId].uid
    knowledgeGroups.exists(g => g.ids.contains(localReplicaId) && g.ids == receivers && g.matches(delta))

}
