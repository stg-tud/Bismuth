package ex2024travel.lofi_acl.travelplanner

import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2024travel.lofi_acl.sync.bft.{BftAclOpGraph, BftInvitation, ReplicaWithBftAcl}
import ex2024travel.lofi_acl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import scalafx.application.{JFXApp3, Platform}

object BftTravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = "Travel Planner"
      scene = new MainScene(BftTpmFactory)
      resizable = true
    }
  }

  override def stopApp(): Unit =
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed

  private object BftTpmFactory extends TravelPlanModelFactory {
    def createAsRootOfTrust: TravelPlanModel = {
      val identity        = IdentityFactory.createNewIdentity
      val aclRoot         = BftAclOpGraph.createSelfSignedRoot(identity)
      val replicaProvider = (new ReplicaWithBftAcl[TravelPlan](_, _, _)).curried(identity)(aclRoot)
      TravelPlanModel(identity, replicaProvider)
    }

    override def createByJoining(invitationString: String): TravelPlanModel = {
      val invitation      = BftInvitation.decode(invitationString)
      val identity        = IdentityFactory.fromIdentityKey(invitation.identityKey)
      val replicaProvider =
        (new ReplicaWithBftAcl[TravelPlan](_, _, _)).curried(identity)(invitation.aclRootOp)
      val travelPlanModel = TravelPlanModel(identity, replicaProvider)
      travelPlanModel.addConnection(invitation.inviter, invitation.joinAddress)
      travelPlanModel
    }
  }
}
