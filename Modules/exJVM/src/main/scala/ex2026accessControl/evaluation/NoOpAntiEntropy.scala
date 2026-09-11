package ex2026accessControl.evaluation

import channels.connection.MessageBuffer
import crypto.Commitment.RevealedValue
import crypto.{Hash, PublicIdentity}
import replication.authz.{AntiEntropy, Replica}

/** An [[AntiEntropy]] subclass that does nothing when any of its methods are invoked. Used to construct a
  * [[Replica]] for evaluation purposes without an actual network layer.
  */
class NoOpAntiEntropy(replica: Replica[?]) extends AntiEntropy(replica, _ => ???, _ => ???) {
  override def listenAddress: Option[(String, Int)]  = None
  override def connect(address: (String, Int)): Unit = ()
  override def start(): Unit                         = ()
  override def stop(): Unit                          = ()

  override def broadcastEvents(events: Iterable[Array[Byte]]): Unit                         = ()
  override def sendEvents(destination: PublicIdentity, events: Iterable[Array[Byte]]): Unit = ()
  override def sendEventsWithDelta(destination: PublicIdentity, eventHashes: Iterable[Hash]): Unit = ()
  override def broadcastDeltasFiltered(deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]): Unit = ()
  override def sendDeltasFiltered(
      destination: PublicIdentity,
      deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]
  ): Unit = ()

  override def receivedMessage(msg: MessageBuffer, sender: PublicIdentity): Unit = ()
  override def requestMissing(): Unit                                           = ()
  override def connectionEstablished(publicIdentity: PublicIdentity): Unit      = ()
  override def connectionShutdown(publicIdentity: PublicIdentity): Unit        = ()
}
