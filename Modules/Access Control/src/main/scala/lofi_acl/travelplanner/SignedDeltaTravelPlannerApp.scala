package lofi_acl.travelplanner

import channels.MessageBuffer
import crypto.channels.{IdentityFactory, PrivateIdentity}
import javafx.scene.input.KeyCode
import lofi_acl.bft.{Acl, AclRdt}
import lofi_acl.sync.signed.{ReplicaOfSignedDeltaRdt, SyncInvitation}
import lofi_acl.sync.{ChannelConnectionManager, MessageReceiver}
import lofi_acl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
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
          println(Debug.shorten(mainScene.tpm.replica.currentAcl.asInstanceOf[Acl]))
          println(Debug.shorten(mainScene.tpm.replica.currentState))
    ): Unit

    val menuBar     = MenuBar()
    val debugMenu   = Menu("Debug")
    val aclMenuItem = MenuItem("Print ACL")
    aclMenuItem.onAction = _ => println(Debug.shorten(mainScene.tpm.replica.currentAcl.asInstanceOf[Acl]))
    val stateMenuItem = MenuItem("Print State")
    stateMenuItem.onAction = _ => println(Debug.shorten(mainScene.tpm.replica.currentState))
    debugMenu.getItems.add(aclMenuItem)
    debugMenu.getItems.add(stateMenuItem)
    menuBar.getMenus.add(debugMenu): Unit
    menuBar.useSystemMenuBar = true
    mainScene.group.children.append(menuBar): Unit
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
