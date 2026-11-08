package webapps.ex2026minisocial

import channels.PlumtreeIO
import rdts.base.Lattice
import rdts.syntax.DeltaBuffer
import reactives.default.*

/** Wraps a [[channels.PlumtreeIO]] such that a chosen CRDT (`MiniSocial`) can be hooked up to a reactive UI,
  * broadcasting local deltas and merging received ones.
  */
object MiniSocialDataManager {

  val (receivedCallback, dataManager: PlumtreeIO[MiniSocial]) = Event.fromCallback {
    PlumtreeIO[MiniSocial](MiniSocialMain.replicaId, payload => Event.handle(payload.data))
  }

  def hookup(init: MiniSocial)(create: (
      DeltaBuffer[MiniSocial],
      Fold.Branch[DeltaBuffer[MiniSocial]]
  ) => Signal[DeltaBuffer[MiniSocial]]): Signal[DeltaBuffer[MiniSocial]] =
    hookup(init, identity, Some.apply)(create)

  def hookup[A: Lattice](
      init: A,
      wrap: A => MiniSocial,
      unwrap: MiniSocial => Option[A]
  )(create: (DeltaBuffer[A], Fold.Branch[DeltaBuffer[A]]) => Signal[DeltaBuffer[A]]): Signal[DeltaBuffer[A]] = {
    dataManager.broadcast(wrap(init))
    val fullInit = dataManager.allPayloads.flatMap(v => unwrap(v.data)).foldLeft(init)(Lattice.merge)

    val branch = Fold.branch[DeltaBuffer[A]] {
      receivedCallback.value.flatMap(unwrap) match
          case None    => Fold.current
          case Some(v) => Fold.current.applyDeltaNonAppend(v)
    }

    val sig = create(DeltaBuffer(fullInit), branch)

    sig.observe { buffer =>
      buffer.deltaBuffer.foreach { delta =>
        dataManager.broadcast(wrap(delta))
      }
    }

    sig
  }

}
