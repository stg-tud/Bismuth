package text_rdt

import org.scalajs.dom.document
import text_rdt.avl.AVLTreeNode
import typings.prosemirrorState.mod.{EditorState, Transaction}
import typings.prosemirrorTransform.mod.ReplaceStep_
import typings.prosemirrorView.mod.{DirectEditorProps, EditorView}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import org.scalajs.dom.URL

final class ProseMirror[F <: FugueFactory](
    elementSelector: String,
    var state: EditorState,
    d3Tree: D3Tree[F],
    replicaState: ReplicaState[d3Tree.factoryContext.type]
) extends Editory {

  private var showTree: Boolean = {
    val url = new URL(
      js.Dynamic.global.window.location.href.asInstanceOf[String]
    )
    url.searchParams.has("graph")
  }

  private val view: EditorView = EditorView(
    org.scalajs.dom.document
      .querySelector(elementSelector)
      .asInstanceOf[typings.prosemirrorView.mod.DOMNode],
    DirectEditorProps(state).setDispatchTransaction(
      ((tr: Transaction) => {
        tr.steps.foreach(step => {
          val stepJson = step
            .toJSON()
            .asInstanceOf[scalajs.js.Dynamic]
          stepJson.stepType
            .asInstanceOf[String] match {
            case "replace" =>
              val replaceStep: ReplaceStep_ = step.asInstanceOf[ReplaceStep_]

              val textBefore =
                tr.docs(0).textBetween(0, replaceStep.from)
              val textWithin =
                tr.docs(0).textBetween(0, replaceStep.to)

              val start = textBefore.size
              val end = textWithin.size

              val text = replaceStep.slice.content
                .textBetween(
                  0,
                  replaceStep.slice.size
                )

              for (i <- end - 1 to start by -1) {
                replicaState.delete(i)
              }

              for ((char, i) <- text.zipWithIndex) {
                replicaState.insert(start + i, char)
              }
            case unknown =>
              println("unknown case")
          }
        })
        state = state.apply(tr)
        view.updateState(state)

        refresh()
      }).asInstanceOf[Any => Unit]
    )
  )

  refresh()

  document
    .querySelector(s"#editor${replicaState.replicaId}-toggle-graph")
    .addEventListener(
      "click",
      click => {
        showTree = !showTree
        refresh()
      }
    )

  override def insert(index: Int, character: Char): Unit = {
    state = state.apply(
      state.tr
        .insertText(character.toString, index, index)
    )
    view.updateState(state)
    refresh()
  }

  override def delete(index: Int): Unit = {
    state = state.apply(
      state.tr
        .delete(index, index + 1)
    )
    view.updateState(state)
    refresh()
  }

  private def refresh(): Unit = {
    if (showTree) {
      val root = replicaState.rootTreeNode.buildTree()
      plotTree(s"#editor${replicaState.replicaId}-graph", root)
      replicaState.factory match {
        case value1: SimpleAVLFugueFactory =>
          plotTree(
            s"#editor${replicaState.replicaId}-graph-avl",
            value1.avlTree.root match {
              case null  => throw IllegalStateException()
              case value => buildTree(value, "")
            }
          )
        case value1: ComplexAVLFugueFactory =>
          plotTree(
            s"#editor${replicaState.replicaId}-graph-avl",
            value1.avlTree.root match {
              case null  => throw IllegalStateException()
              case value => buildTreeComplex(value, "")
            }
          )
        case _ =>
      }
    } else {
      document
        .querySelector(s"#editor${replicaState.replicaId}-graph")
        .innerHTML = "";
      document
        .querySelector(s"#editor${replicaState.replicaId}-graph-avl")
        .innerHTML = "";
    }
  }

  private def ttComplex(
      treeNode: AVLTreeNode[ComplexAVLTreeNode]
  ): String = {
    s"${treeNode.value._values match {
        case null  => "⌫".repeat(treeNode.value.to - treeNode.value.offset + 1)
        case value => treeNode.value.values.mkString
      }} ${treeNode.value.rid match {
        case null => "root"
        case rid =>
          s"${rid}#${treeNode.value.counter}.${treeNode.value.offset}-${treeNode.value.to}"
      }}" + " size: " + treeNode.deepSize
  }

  private def buildTreeComplex(
      treeNode: AVLTreeNode[ComplexAVLTreeNode],
      prefix: String
  ): MyD3TreeNode = {
    MyD3TreeNode(
      ttComplex(treeNode),
      treeNode.left match {
        case null => Array()
        case value =>
          Array(buildTreeComplex(value, prefix + ttComplex(treeNode) + "/"))
      },
      treeNode.right match {
        case null => Array()
        case value =>
          Array(buildTreeComplex(value, prefix + ttComplex(treeNode) + "/"))
      }
    )
  }

  private def tt(treeNode: AVLTreeNode[SimpleAVLTreeNode]): String = {
    (treeNode.value.value match {
      case null  => "⌫"
      case value => value.toString
    }) + " " + (treeNode.value.id match {
      case null  => "root"
      case value => s"${value.rid}#${value.counter}"
    }) + " size: " + treeNode.deepSize
  }

  private def buildTree(
      treeNode: AVLTreeNode[SimpleAVLTreeNode],
      prefix: String
  ): MyD3TreeNode = {
    MyD3TreeNode(
      tt(treeNode),
      treeNode.left match {
        case null  => Array()
        case value => Array(buildTree(value, prefix + tt(treeNode) + "/"))
      },
      treeNode.right match {
        case null  => Array()
        case value => Array(buildTree(value, prefix + tt(treeNode) + "/"))
      }
    )
  }
}
