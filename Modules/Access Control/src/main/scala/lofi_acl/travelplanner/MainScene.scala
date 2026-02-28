package lofi_acl.travelplanner

import javafx.stage.Stage
import lofi_acl.travelplanner.model.{TravelPlanModel, TravelPlanModelFactory}
import lofi_acl.travelplanner.view.TravelPlanView
import lofi_acl.travelplanner.viewmodel.TravelPlanViewModel
import scalafx.application.Platform
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, HBox, Priority, VBox}
import scalafx.scene.{Group, Scene}

import scala.concurrent.ExecutionContext.global

// Initial screen with creation / join functionality
class MainScene(travelPlanModelFactory: TravelPlanModelFactory) extends Scene {
  var tpm: TravelPlanModel = null
  val group: Group         = Group()

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  private val createNewDocumentButton: Button = new Button("Create new Travel Plan Document")
  createNewDocumentButton.alignment = Pos.Center
  createNewDocumentButton.onAction() = _ => createNewDocumentButtonPressed()
  createNewDocumentButton.disable <== documentIsOpen

  private val invitationTextField = new TextField {
    promptText = "paste invitation here"
  }
  invitationTextField.disable <== documentIsOpen

  private val joinDocumentButton: Button = new Button("Join")
  joinDocumentButton.alignment = Pos.Center
  joinDocumentButton.onAction() = _ => joinDocumentButtonPressed()
  joinDocumentButton.disable <== documentIsOpen || invitationTextField.text.isEmpty

  val rootPane = new BorderPane()
  rootPane.center = VBox(
    createNewDocumentButton,
    HBox(
      invitationTextField,
      joinDocumentButton
    )
  )
  group.children.append(rootPane)
  content = group

  private def createNewDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    global.execute { () =>
      init(travelPlanModelFactory.createAsRootOfTrustWithExampleData)
    }
  }

  private def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    val inviteString = invitationTextField.getText
    global.execute { () =>
      init(travelPlanModelFactory.createByJoining(inviteString))
    }
  }

  private def init(travelPlanModel: TravelPlanModel): Unit = {
    this.tpm = travelPlanModel
    Platform.runLater {
      val travelPlanViewModel = TravelPlanViewModel(travelPlanModel)
      group.children.replaceAll(
        rootPane,
        new HBox {
          children = TravelPlanView(travelPlanViewModel)
          hgrow = Priority.Always
        }
      )
      val stage = window.get().asInstanceOf[Stage]
      stage.sizeToScene()
      stage.setTitle(s"TravelPlanner - replica ${travelPlanModel.publicId.id.take(10)}")
    }
  }
}
