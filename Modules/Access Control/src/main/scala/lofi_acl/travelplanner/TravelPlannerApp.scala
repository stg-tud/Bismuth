package lofi_acl.travelplanner

import crypto.channels.IdentityFactory
import lofi_acl.sync.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.monotonic.SyncWithMonotonicAcl.messageJsonCodec
import lofi_acl.sync.monotonic.{MonotonicAcl, MonotonicInvitation, SyncWithMonotonicAcl}
import lofi_acl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import scalafx.application.{JFXApp3, Platform}

object TravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = "Travel Planner"
      scene = new MainScene(MonotonicTpmFactory)
      resizable = true
    }
  }

  override def stopApp(): Unit =
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed

  private object MonotonicTpmFactory extends TravelPlanModelFactory {
    def createAsRootOfTrust: TravelPlanModel = {
      val identity                           = IdentityFactory.createNewIdentity
      val rootAclDelta: AclDelta[TravelPlan] = MonotonicAcl.createRootOfTrust[TravelPlan](identity)
      val syncProvider                       =
        (new SyncWithMonotonicAcl[TravelPlan](_, _, _, _)).curried(identity)(identity.getPublic)(List(rootAclDelta))
      TravelPlanModel(identity, syncProvider)
    }

    def createByJoining(inviteString: String): TravelPlanModel = {
      val invite       = MonotonicInvitation.decode(inviteString)
      val identity     = IdentityFactory.fromIdentityKey(invite.identityKey)
      val syncProvider =
        (new SyncWithMonotonicAcl[TravelPlan](_, _, _, _)).curried(identity)(invite.rootOfTrust)(List.empty)
      val travelPlanModel = TravelPlanModel(identity, syncProvider)
      travelPlanModel.addConnection(invite.inviter, invite.joinAddress)
      travelPlanModel
    }
  }
}
