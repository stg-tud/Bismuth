package lofi_acl.evaluation.centralized

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.Debug
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.sync.anti_entropy.{AclAntiEntropy, AntiEntropyCommunicator, FilteredRdtAntiEntropy, SignedDelta}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots

class FilteringForwarderRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    localIdentity: PrivateIdentity,
    network: AntiEntropyCommunicator[State],
    aclAntiEntropy: AclAntiEntropy,
    onDeltasReceived: Dots => Unit = _ => ()
)(using Encoder[SignedDelta[State]])
    extends FilteredRdtAntiEntropy[State](localIdentity, (_, _) => (), network, aclAntiEntropy) {

  override def localMutation(mutator: State => State): Unit = throw UnsupportedOperationException()

  override protected def applyVerifiedDeltas(source: PublicIdentity, deltas: Seq[SignedDelta[State]]): Unit = {
    // TODO: currently merges update into the local state
    var dots: Dots = Dots.empty
    deltas.foreach { delta =>
      deltaStore.put(delta.dot, delta)
      dots = dots.add(delta.dot)
    }

    if dots.nonEmpty then {
      currentStateRef.updateAndGet((oldDots, ignored) =>
        (oldDots.union(dots), ignored) // does not merge into state
      )
      missingDots.updateAndGet(missing => missing.diff(dots))
      filteredDots.updateAndGet(filtered => filtered.diff(dots))

      Debug.log(s"Broadcast: ${Debug.shorten(Dots.from(deltas.map(_.dot)))}")

      // Broadcast deltas filtered (but not back to source)
      network
        .connectedPeers
        .filterNot(_ == source)
        .foreach { remote =>
          sendDeltasFiltered(deltas, remote)
        }

      onDeltasReceived(dots)
    }
  }

}
