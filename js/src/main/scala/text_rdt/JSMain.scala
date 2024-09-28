package text_rdt

import org.scalablytyped.runtime.StringDictionary
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.URL
import org.scalajs.dom.document
import typings.prosemirrorKeymap.mod.keymap
import typings.prosemirrorModel.mod.NodeSpec
import typings.prosemirrorModel.mod.Schema
import typings.prosemirrorModel.mod.SchemaSpec
import typings.prosemirrorModel.mod.TagParseRule
import typings.prosemirrorModel.prosemirrorModelStrings.full
import typings.prosemirrorState.mod.Command
import typings.prosemirrorState.mod.EditorState
import typings.prosemirrorState.mod.EditorStateConfig
import typings.prosemirrorState.mod.Plugin
import upickle.default.*

import scala.collection.immutable
import scala.scalajs.js

val schema = Schema[Any, Any](
  SchemaSpec(
    typings.orderedmap.mod.default.from(
      StringDictionary(
        ("text", NodeSpec()),
        (
          "doc",
          NodeSpec()
            .setContent("text*")
            .setMarks("")
            .setCode(true)
            .setDefining(true)
            .setParseDOM(
              js.Array(TagParseRule("pre").setPreserveWhitespace(full))
            )
            .setToDOM(_ => Array("pre", 0))
        )
      )
    )
  )
)

val hardBreakCommand: Command = (state, dispatch, _) => {
  dispatch.get(
    state.tr.insertText("\n")
  )
  true
}

val editorStateConfig = EditorStateConfig()
  .setPluginsVarargs(
    keymap(
      StringDictionary(
        ("Enter", hardBreakCommand)
      )
    )
  )
  .setSchema(
    schema
  )

object JSMain {

  @main
  def main(): Unit = {
    org.scalajs.dom.window.onerror =
      (message, source, lineNumber, colno, error) =>
        {
          org.scalajs.dom.window
            .alert(s"UNHANDLED ERROR: ${message}");
        };

    val url = new URL(
      document.location.href.asInstanceOf[String]
    )
    if (url.searchParams.has("simple")) {
      val _ = JSMain(simpleTreeNodeD3Tree)
    } else if (url.searchParams.has("complex")) {
      val _ = JSMain(complexTreeNodeD3Tree)
    } else if (url.searchParams.has("simpleavl")) {
      val _ = JSMain(simpleAVLTreeNodeD3Tree)
    } else if (url.searchParams.has("complexavl")) {
      val _ = JSMain(complexAVLTreeNodeD3Tree)
    } else if (url.searchParams.has("p2p")) {
      val _ = JSP2P()
    } else if (url.searchParams.has("doc")) {
      val _ = JSP2PAutomatic()
    } else if (url.searchParams.has("tree")) {
      plotTree(
        s"body",
        read(url.searchParams.get("tree"))
      )
      val node = document.querySelector("svg")
      document.body
        .appendChild(document.createElement("style"))
        .asInstanceOf[HTMLElement]
        .innerHTML = """
          body {
            margin: 0;
          }
          svg {
            display: block; /* block breaks page overflow */
          }"""
      document.body
        .appendChild(document.createElement("style"))
        .asInstanceOf[HTMLElement]
        .innerHTML = s"""
          @media print {
            @page {
                size: ${node.getBoundingClientRect().width}px ${node
          .getBoundingClientRect()
          .height}px;
                margin: 0;
            }
          }"""
    } else {
      document.body.innerHTML = """
        <p>Go to <a href="?p2p">?p2p</a> for the peer to peer collaborative editing application.</p>

        <p>Go to <a href="?simple">?simple</a> for the simple algorithm, <a href="?complex">?complex</a> for the complex algorithm, <a href="?simpleavl">?simpleavl</a> for the simple avl algorithm, <a href="?complexavl">?complexavl</a> for the complex avl algorithm.</p>

        <p>For debugging go to <a href="?simple&graph">?simple&graph</a> for the simple algorithm, <a href="?complex&graph">?complex&graph</a> for the complex algorithm, <a href="?simpleavl&graph">?simpleavl&graph</a> for the simple avl algorithm, <a href="?complexavl&graph">?complexavl&graph</a> for the complex avl algorithm.</p>
        """
    }
  }
}

private case class JSMain[F <: FugueFactory](
    d3Tree: D3Tree[F]
) {
  private var editors: Seq[Replica[d3Tree.factoryContext.type]] =
    List[
      Replica[d3Tree.factoryContext.type]
    ]()

  org.scalajs.dom.document.body.innerHTML = """
  <button id="create-editor">Create editor</button>

  <div id="container" style="display: grid; grid-template-columns: 1fr 1fr;">

  </div>

  <textarea
    id="example-text">Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</textarea>
  """

  org.scalajs.dom.document
    .querySelector("#create-editor")
    .addEventListener(
      "click",
      event => {
        event.preventDefault()

        addEditor()
      }
    )

  document.body
    .addEventListener(
      "click",
      event => {
        val element = event.target.asInstanceOf[Element]
        if (element.classList.contains("sync")) {
          event.preventDefault()

          val dataReplica1 =
            element
              .asInstanceOf[js.Dynamic]
              .dataset
              .replica1
              .asInstanceOf[String]
              .toInt
          val dataReplica2 =
            element
              .asInstanceOf[js.Dynamic]
              .dataset
              .replica2
              .asInstanceOf[String]
              .toInt

          val replica1 = editors(dataReplica1)
          val replica2 = editors(dataReplica2)

          replica1.sync(replica2)
        }
      }
    )

  private def addEditor(): Unit = {
    val newEditor = document.createElement("div")
    newEditor.id = s"editor${editors.length}-parent"
    val child1 = document.createElement("h3")
    child1.innerText = s"Editor ${editors.length}"
    val child2 = document.createElement("div")
    child2.classList.add("my-border")
    child2.id = s"editor${editors.length}"
    val child3 = document.createElement("pre")
    child3.id = s"editor${editors.length}-info"
    val child4 = document.createElement("button")
    child4.id = s"editor${editors.length}-toggle-graph"
    child4.innerText = "Toggle graphs"
    val child5 = document.createElement("div")
    child5.id = s"editor${editors.length}-graph"
    val child6 = document.createElement("div")
    child6.id = s"editor${editors.length}-graph-avl"
    val _ = newEditor.appendChild(child1)
    val _ = newEditor.appendChild(child2)
    val _ = newEditor.appendChild(child3)
    val _ = newEditor.appendChild(child4)
    val _ = newEditor.appendChild(child5)
    val _ = newEditor.appendChild(child6)

    for (i <- editors.indices) {
      {
        val button = document.createElement("button")
        button.id = s"sync-editor$i-editor${editors.length}"
        button.classList.add("sync")
        button.textContent = s"Sync with $i"
        button.asInstanceOf[js.Dynamic].dataset.replica1 = i
        button.asInstanceOf[js.Dynamic].dataset.replica2 = editors.length
        val _ = newEditor
          .appendChild(button)
      }
      {
        val button = document.createElement("button")
        button.id = s"sync-editor${editors.length}-editor$i"
        button.classList.add("sync")
        button.textContent = s"Sync with ${editors.length}"
        button.asInstanceOf[js.Dynamic].dataset.replica1 = editors.length
        button.asInstanceOf[js.Dynamic].dataset.replica2 = i
        document.querySelector(s"#editor$i-parent").appendChild(button)
      }
    }

    val _ = org.scalajs.dom.document
      .querySelector("#container")
      .appendChild(newEditor)

    val editor = createEditor(s"${editors.length}")
    editors = editors.appended(editor)
  }

  private def createEditor(
      name: String
  ): Replica[d3Tree.factoryContext.type] = {
    val replicaState =
      new ReplicaState[d3Tree.factoryContext.type](
        name
      )(using d3Tree.factoryContext)

    val state = EditorState.create(editorStateConfig)

    val prosemirror =
      ProseMirror(s"#editor$name", state, d3Tree, replicaState)

    val replica =
      Replica[d3Tree.factoryContext.type](replicaState, prosemirror)

    replica
  }
}
