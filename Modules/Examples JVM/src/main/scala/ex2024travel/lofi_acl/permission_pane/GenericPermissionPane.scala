package ex2024travel.lofi_acl.permission_pane

import javafx.scene.control as jfxsc
import javafx.util as jfxu
import scalafx.Includes.jfxTreeItem2sfx
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.BeanIncludes.jfxObservableValue2sfx
import scalafx.geometry.Pos.Center
import scalafx.scene.control.*
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.scene.{Group, Scene}

class GenericPermissionPane extends GridPane {}

object TreeTableViewExample extends JFXApp3 {

  private def populateTreeItem(rootSelector: ArdtPermissionSelector[?]): TreeItem[PermissionSelectionViewModel] = {
    val model = PermissionSelectionViewModel.fromSelector(rootSelector)
    val root  = TreeItem(model)
    root.expanded = true
    populateChildren(root, model)

    def populateChildren(item: TreeItem[PermissionSelectionViewModel], model: PermissionSelectionViewModel): Unit = {
      val children = model.children.values.toSeq
      item.children = children.map(childModel => TreeItem(childModel))
      item.children.zip(children).foreach((childItem, childModel) => populateChildren(childItem, childModel))
    }

    root
  }

  override def start(): Unit = {
    val sceneRoot = Group()
    val scene     = Scene(sceneRoot) // , 200, 400)
    Platform.implicitExit = true

    stage = new JFXApp3.PrimaryStage {
      title = s"Permission Pane"
      resizable = true
    }
    stage.setTitle("Permission Selection")

    val rootSelector = ArdtPermissionSelector(
      "*",
      Map(
        "test" -> ArdtPermissionSelector("test"),
        "abc"  -> ArdtPermissionSelector(
          "abc",
          Map(
            "*"  -> ArdtPermissionSelector("*", Map("a" -> ArdtPermissionSelector("a"))),
            "42" -> ArdtPermissionSelector("42", Map("a" -> ArdtPermissionSelector("a"))),
            "21" -> ArdtPermissionSelector("21", Map("a" -> ArdtPermissionSelector("a"))),
          ),
        )
      )
    )

    val labelColumn = TreeTableColumn[PermissionSelectionViewModel, String]("Label")
    labelColumn.setPrefWidth(150)
    labelColumn.cellValueFactory = { paneItem => paneItem.value.value.map(_.uiText) }

    val readPermColumn = TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel]("read")
    readPermColumn.resizable = false
    readPermColumn.cellValueFactory = { paneItem => paneItem.value.value }
    readPermColumn.cellFactory = new jfxu.Callback[
      jfxsc.TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel],
      jfxsc.TreeTableCell[PermissionSelectionViewModel, PermissionSelectionViewModel]
    ] {
      def call(p: jfxsc.TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel])
          : jfxsc.TreeTableCell[PermissionSelectionViewModel, PermissionSelectionViewModel] =
        new CustomCheckBoxTreeTableCell(_.readCheckbox)
    }
    readPermColumn.setPrefWidth(50)

    val writePermColumn = TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel]("write")
    writePermColumn.resizable = false
    writePermColumn.cellValueFactory = { paneItem => paneItem.value.value }
    writePermColumn.cellFactory = new jfxu.Callback[
      jfxsc.TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel],
      jfxsc.TreeTableCell[PermissionSelectionViewModel, PermissionSelectionViewModel]
    ] {
      def call(p: jfxsc.TreeTableColumn[PermissionSelectionViewModel, PermissionSelectionViewModel])
          : jfxsc.TreeTableCell[PermissionSelectionViewModel, PermissionSelectionViewModel] =
        new CustomCheckBoxTreeTableCell(_.writeCheckbox)
    }
    writePermColumn.setPrefWidth(50)

    val view = TreeTableView(populateTreeItem(rootSelector))
    view.columns ++= Seq(labelColumn, readPermColumn, writePermColumn)
    view.setShowRoot(true)
    sceneRoot.getChildren.add(view)
    stage.scene = scene
    stage.show()
  }
}

class CustomCheckBoxTreeTableCell(checkBoxSource: PermissionSelectionViewModel => CheckBox)
    extends jfxsc.TreeTableCell[PermissionSelectionViewModel, PermissionSelectionViewModel] {
  override def updateItem(viewModel: PermissionSelectionViewModel, empty: Boolean): Unit = {
    if empty || viewModel == null then {
      setGraphic(null)
    } else {
      val box = VBox(checkBoxSource(viewModel))
      box.alignment = Center
      setGraphic(box)
    }
  }
}
