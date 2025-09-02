package ex2024travel.lofi_acl.sync.bft

import crypto.channels.{IdentityFactory, PrivateIdentity}
import munit.FunSuite

class BftInvitationTest extends FunSuite {
  test("decode(encode(a)) = a") {
    val identity: PrivateIdentity = IdentityFactory.createNewIdentity
    val aclRoot                   = BftAclOpGraph.createSelfSignedRoot(identity)
    val invitation                = BftInvitation(aclRoot, identity.identityKey, identity.getPublic, "localhost:4242")
    val decoded                   = BftInvitation.decode(invitation.encode)

    println(decoded.aclRootOp.decode)
    assertEquals(decoded.aclRootOp.decode, invitation.aclRootOp.decode)
    assertEquals(decoded.identityKey.getPublic, invitation.identityKey.getPublic)
    assertEquals(decoded.identityKey.getPrivate, invitation.identityKey.getPrivate)
    assertEquals(decoded.inviter, invitation.inviter)
    assertEquals(decoded.joinAddress, invitation.joinAddress)
  }
}
