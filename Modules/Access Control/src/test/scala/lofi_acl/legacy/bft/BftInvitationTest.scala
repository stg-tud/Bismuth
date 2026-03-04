package lofi_acl.legacy.bft

import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.legacy.bft.{BftAclOpGraph, BftInvitation}
import munit.FunSuite

class BftInvitationTest extends FunSuite {
  test("decode(encode(a)) = a") {
    val identity: PrivateIdentity = IdentityFactory.createNewIdentity
    val aclRoot                   = BftAclOpGraph.createSelfSignedRoot(identity)
    val invitation                = BftInvitation(aclRoot, identity.identityKey, identity.getPublic, "localhost:4242")
    val decoded                   = BftInvitation.decode(invitation.encode)

    println(decoded.aclRootOp.deserialize)
    assertEquals(decoded.aclRootOp.deserialize, invitation.aclRootOp.deserialize)
    assertEquals(decoded.identityKey.getPublic, invitation.identityKey.getPublic)
    assertEquals(decoded.identityKey.getPrivate, invitation.identityKey.getPrivate)
    assertEquals(decoded.inviter, invitation.inviter)
    assertEquals(decoded.joinAddress, invitation.joinAddress)
  }
}
