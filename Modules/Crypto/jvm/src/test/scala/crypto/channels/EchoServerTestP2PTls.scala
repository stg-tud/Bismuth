package crypto.channels

import crypto.channels.{IdentityFactory, PrivateIdentity}
import channels.EchoCommunicationTest

class EchoServerTestP2PTls extends EchoCommunicationTest(
      ec => {
        val latentConnection = EchoServerTestP2PTls.p2pTls1.latentListener(0, ec)
        (latentConnection.listenPort, latentConnection)
      },
      ec => port => EchoServerTestP2PTls.p2pTls2.latentConnect("localhost", port, ec)
    )

object EchoServerTestP2PTls {
  val id1: PrivateIdentity = IdentityFactory.createNewIdentity
  val id2: PrivateIdentity = IdentityFactory.createNewIdentity
  val p2pTls1              = P2PTls(id1.tlsKeyPem, id1.tlsCertPem)
  val p2pTls2              = P2PTls(id2.tlsKeyPem, id2.tlsCertPem)
}
