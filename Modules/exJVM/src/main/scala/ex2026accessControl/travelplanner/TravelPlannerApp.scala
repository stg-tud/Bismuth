package ex2026accessControl.travelplanner

import crypto.channels.IdentityFactory
import ex2026accessControl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import ex2026accessControl.travelplanner.{MainScene, TravelPlan}
import replication.authz.Authorization
import scalafx.application.{JFXApp3, Platform}

object TravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    val mainScene = new MainScene(TpmFactory)
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = "Travel Planner"
      scene = mainScene
      resizable = true
    }
  }

  override def stopApp(): Unit =
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed

  private object TpmFactory extends TravelPlanModelFactory {
    def createAsRootOfTrust: TravelPlanModel = {
      val identity     = IdentityFactory.createNewIdentity
      val genesisEvent = Authorization.createGenesis(identity)
      val syncProvider = (onDeltaReceive: TravelPlan => Unit) =>
        new SyncImpl[TravelPlan](identity, genesisEvent.hash, Some(genesisEvent))
      TravelPlanModel(identity, syncProvider)
    }

    override def createByJoining(invitationString: String): TravelPlanModel = {
      val invitation   = SyncInvitation.decode(invitationString)
      val identity     = IdentityFactory.fromIdentityKey(invitation.identityKey)
      val syncProvider = (onDeltaReceive: (tp: TravelPlan) => Unit) =>
        new SyncImpl[TravelPlan](identity, invitation.genesis)
      val travelPlanModel = TravelPlanModel(identity, syncProvider)
      travelPlanModel.addConnection(invitation.inviter, invitation.joinAddress)
      travelPlanModel
    }
  }

}
