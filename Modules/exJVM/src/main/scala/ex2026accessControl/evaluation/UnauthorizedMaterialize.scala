package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import crypto.Commitment.RevealedValue
import rdts.base.{Bottom, Lattice}
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEvent, ArdtEventGraph, DeltaValueStore}

import scala.reflect.ClassTag

/** A baseline for [[replication.authz.Authorization.materialize]] that merges every delta value found in the
  * given [[DeltaValueStore]] without any access control checks (no capability/write-permission filtering, no
  * revocation/causality checks). Used to isolate the computational overhead that access control enforcement adds
  * on top of plain RDT state materialization.
  */
object UnauthorizedMaterialize {
  def materialize[T: {Lattice, Bottom, JsonValueCodec, ClassTag}](
      eventGraph: ArdtEventGraph[T],
      deltaValueStore: DeltaValueStore[T]
  ): T =
      val deltas = Array.ofDim[T](eventGraph.nextEventIndex)
      eventGraph.events.foreach {
        case (_, (deltaEvent @ ArdtEvent(DeltaCommitment(commitmentHash), _, _, _, _), causalOrderIndex)) =>
          deltaValueStore.get(commitmentHash) match {
            case Some(RevealedValue(encodedDelta, _)) =>
              deltas(causalOrderIndex) = readFromArray[T](encodedDelta)
            case _ =>
          }
        case _ =>
      }
      deltas.foldLeft(Bottom.empty)((acc, value) =>
        if value == null then acc
        else acc.merge(value)
      )
}
