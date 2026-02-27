package lofi_acl.travelplanner

import channels.MessageBuffer
import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.bft.AclRdt
import lofi_acl.sync.signed.{ReplicaOfSignedDeltaRdt, SyncInvitation}
import lofi_acl.sync.{ChannelConnectionManager, MessageReceiver}
import lofi_acl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import scalafx.application.{JFXApp3, Platform}

object SignedDeltaTravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = "Travel Planner"
      scene = new MainScene(TpmFactory)
      resizable = true
    }
  }

  override def stopApp(): Unit =
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed

  private val connManProvider = (id: PrivateIdentity, msgRec: MessageReceiver[MessageBuffer]) =>
    ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, msgRec)

  private object TpmFactory extends TravelPlanModelFactory {
    def createAsRootOfTrust: TravelPlanModel = {
      val identity        = IdentityFactory.createNewIdentity
      val aclRoot         = AclRdt.createSelfSignedRoot(identity)
      val replicaProvider = (onDeltaReceive: (tp: TravelPlan) => Unit) =>
        new ReplicaOfSignedDeltaRdt[TravelPlan](identity, connManProvider, aclRoot, onDeltaReceive)
      TravelPlanModel(identity, replicaProvider)
    }

    override def createByJoining(invitationString: String): TravelPlanModel = {
      val invitation      = SyncInvitation.decode(invitationString)
      val aclRoot         = invitation.rootOp
      val identity        = IdentityFactory.fromIdentityKey(invitation.identityKey)
      val replicaProvider = (onDeltaReceive: (tp: TravelPlan) => Unit) =>
        new ReplicaOfSignedDeltaRdt[TravelPlan](identity, connManProvider, aclRoot, onDeltaReceive)
      val travelPlanModel = TravelPlanModel(identity, replicaProvider)
      travelPlanModel.addConnection(invitation.inviter, invitation.joinAddress)
      travelPlanModel
    }
  }

}
