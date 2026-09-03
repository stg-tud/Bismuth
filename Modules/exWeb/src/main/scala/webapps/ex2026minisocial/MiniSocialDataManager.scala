package webapps.ex2026minisocial

import channels.BroadcastIO
import rdts.base.Lattice
import rdts.syntax.DeltaBuffer
import reactives.default.*

/** Wraps a [[BroadcastIO]] such that a chosen CRDT (`MiniSocial`) can be hooked up to a reactive UI,
  * broadcasting local deltas and merging received ones.
  */
object MiniSocialDataManager {

  val (receivedCallback, dataManager: BroadcastIO[MiniSocial]) = Event.fromCallback {
    BroadcastIO[MiniSocial](MiniSocialMain.replicaId, Event.handle)
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
    dataManager.lock.synchronized {
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

}