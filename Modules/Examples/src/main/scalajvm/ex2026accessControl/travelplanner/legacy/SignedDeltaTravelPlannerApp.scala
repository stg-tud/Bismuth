package ex2026accessControl.travelplanner.legacy

import channels.connection.MessageBuffer
import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026accessControl.Debug
import ex2026accessControl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import ex2026accessControl.travelplanner.{MainScene, TravelPlan}
import javafx.scene.input.KeyCode
import replication.acl.{Acl, AclRdt}
import replication.sync.{ChannelConnectionManager, MessageReceiver}
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.input.KeyCodeCombination

object SignedDeltaTravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    val mainScene = new MainScene(TpmFactory)
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = "Travel Planner"
      scene = mainScene
      resizable = true
    }
    val accelerators = stage.scene.value.getAccelerators
    val f1           = KeyCodeCombination(KeyCode.F1)
    accelerators.put(
      f1,
      () =>
          println(Debug.shorten(mainScene.tpm.sync.availablePermissions.asInstanceOf[Acl]))
          println(Debug.shorten(mainScene.tpm.sync.currentState))
    ): Unit

    mainScene.group.children.append(
      debugMenuBar(mainScene.tpm.sync.asInstanceOf[SyncOfSignedDeltaRdt[TravelPlan]])
    ): Unit
  }

  private def debugMenuBar(replica: => SyncOfSignedDeltaRdt[TravelPlan]): MenuBar = {
    val menuBar       = MenuBar()
    val debugMenu     = Menu("Debug")
    val stateMenuItem = MenuItem("Print State")
    stateMenuItem.onAction = _ => println(Debug.shorten(replica.currentState))
    val metaDataMenuItem = MenuItem("Print Metadata")
    metaDataMenuItem.onAction = _ =>
        println(replica.sync.aclVersion.map(Debug.shorten))
        println(Debug.shorten(replica.sync.stateVersion))
    val connectedPeersMenuItem = MenuItem("Print Connected Replicas")
    connectedPeersMenuItem.onAction = _ =>
      println(replica.sync.connectedPeers.map(Debug.shorten))
    debugMenu.getItems.addAll(stateMenuItem, metaDataMenuItem, connectedPeersMenuItem)
    menuBar.getMenus.add(debugMenu): Unit
    menuBar.useSystemMenuBar = true
    menuBar
  }

  override def stopApp(): Unit =
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed

  private val connManProvider = (id: PrivateIdentity, msgRec: MessageReceiver[MessageBuffer]) =>
    ChannelConnectionManager(id, msgRec, disableLogging = false)

  private object TpmFactory extends TravelPlanModelFactory {
    def createAsRootOfTrust: TravelPlanModel = {
      val identity        = IdentityFactory.createNewIdentity
      val aclRoot         = AclRdt.createSelfSignedRoot(identity)
      val replicaProvider = (onDeltaReceive: TravelPlan => Unit) =>
        new SyncOfSignedDeltaRdt[TravelPlan](
          identity,
          connManProvider,
          aclRoot,
          (_, tp: TravelPlan) => onDeltaReceive(tp)
        )
      TravelPlanModel(identity, replicaProvider)
    }

    override def createByJoining(invitationString: String): TravelPlanModel = {
      val invitation      = AclSyncInvitation.decode(invitationString)
      val aclRoot         = invitation.rootOp
      val identity        = IdentityFactory.fromIdentityKey(invitation.identityKey)
      val replicaProvider = (onDeltaReceive: (tp: TravelPlan) => Unit) =>
        new SyncOfSignedDeltaRdt[TravelPlan](
          identity,
          connManProvider,
          aclRoot,
          (_, tp: TravelPlan) => onDeltaReceive(tp)
        )
      val travelPlanModel = TravelPlanModel(identity, replicaProvider)
      travelPlanModel.addConnection(invitation.inviter, invitation.joinAddress)
      travelPlanModel
    }
  }

}
