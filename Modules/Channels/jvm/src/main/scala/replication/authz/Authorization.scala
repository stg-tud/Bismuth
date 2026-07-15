package replication.authz

import crypto.{Hash, PublicIdentity}

object Authorization {
  def materialize[T](eventGraph: EventGraph[T], deltaValueStore: DeltaValueStore[T]): T = ???

  def mayRead[T](replica: PublicIdentity, delta: Hash, eventGraph: EventGraph[T]): Option[Boolean] = ???

  def mayWrite[T](eventGraph: EventGraph[T]): Boolean = ???

  def mayRevoke(revocationEvent: ArdtEvent, otherEvent: Hash, eventGraph: EventGraph[?]): Boolean = ???
}
