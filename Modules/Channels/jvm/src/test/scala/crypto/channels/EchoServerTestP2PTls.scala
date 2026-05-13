package crypto.channels

import channels.EchoCommunicationTest
import crypto.channels.{IdentityFactory, PrivateIdentity}

class EchoServerTestP2PTls extends EchoCommunicationTest[channels.ConnectionDescriptor.Tcp](
      (ec, _) => EchoServerTestP2PTls.p2pTls1.latentListener(ec),
      (ec, _) =>
        descriptor =>
          EchoServerTestP2PTls.p2pTls2.latentConnect(descriptor.host, descriptor.port, ec)
    )

object EchoServerTestP2PTls {
  val id1: PrivateIdentity = IdentityFactory.createNewIdentity
  val id2: PrivateIdentity = IdentityFactory.createNewIdentity
  val p2pTls1: P2PTls      = P2PTls(id1)
  val p2pTls2: P2PTls      = P2PTls(id2)
}
