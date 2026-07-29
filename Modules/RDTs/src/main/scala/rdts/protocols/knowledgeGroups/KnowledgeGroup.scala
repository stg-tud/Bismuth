package rdts.protocols.knowledgeGroups

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, LocalUid, Uid}

case class KnowledgeGroup[A](ids: Set[Uid], path: A => A, enabled: A => Boolean) {
  def matches(delta: A)(using Bottom[A]): Boolean = {
    val pathDelta = path(delta)
    enabled(pathDelta) && !Bottom[A].isEmpty(pathDelta)
  }
}

case class PrdtSystem[A](knowledgeGroups: Set[KnowledgeGroup[A]]) {
  def groupsFor(uid: Uid) =
    knowledgeGroups.filter(_.ids.contains(uid))

  def matches(delta: A, id: Uid)(using Bottom[A], LocalUid) =
    knowledgeGroups.exists(g => g.ids.contains(replicaId) && g.ids.contains(id) && g.matches(delta))
}
