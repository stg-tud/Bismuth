package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import rdts.base.{Bottom, Lattice}
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEvent, ArdtEventGraph, DeltaValueStore}

/** A baseline for [[replication.authz.Authorization.materialize]] that merges every delta value found in the
  * given [[DeltaValueStore]] without any access control checks (no capability/write-permission filtering, no
  * revocation/causality checks). Used to isolate the computational overhead that access control enforcement adds
  * on top of plain RDT state materialization.
  */
object UnauthorizedMaterialize {
  def materialize[T: {Lattice, Bottom, JsonValueCodec}](
      eventGraph: ArdtEventGraph[T],
      deltaValueStore: DeltaValueStore[T]
  ): T =
    eventGraph.events.iterator.foldLeft(Bottom[T].empty) {
      case (left, (_, (ArdtEvent(DeltaCommitment(commitment), _, _, _, _), _))) =>
        deltaValueStore.get(commitment)
          .map(committed => readFromArray[T](committed.value))
          .map(left.merge)
          .getOrElse(left)
      case (left, _) => left
    }
}
