package webapps.ex2024textRDT

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import org.scalablytyped.runtime.StringDictionary
import org.scalajs.dom.{Element, HTMLElement, URL, document}
import text_rdt.*
import text_rdt.debug.{*, given}
import text_rdt.fugue.complex.{*, given}
import text_rdt.fugue.complexavl.{*, given}
import text_rdt.fugue.simple.{*, given}
import text_rdt.fugue.simpleavl.{*, given}
import text_rdt.fugue.{*, given}
import text_rdt.given
import typings.prosemirrorKeymap.mod.keymap
import typings.prosemirrorModel.mod.{NodeSpec, Schema, SchemaSpec, TagParseRule}
import typings.prosemirrorModel.prosemirrorModelStrings.full
import typings.prosemirrorState.mod.{Command, EditorState, EditorStateConfig, Plugin}

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
    if url.searchParams.has("simple") then {
      val _ = JSMain(simpleTreeNodeD3Tree)
    } else if url.searchParams.has("complex") then {
      val _ = JSMain(complexTreeNodeD3Tree)
    } else if url.searchParams.has("simpleavl") then {
      val _ = JSMain(simpleAVLTreeNodeD3Tree)
    } else if url.searchParams.has("complexavl") then {
      val _ = JSMain(complexAVLTreeNodeD3Tree)
    } else if url.searchParams.has("p2p") then {
      val _ = JSP2P()
    } else if url.searchParams.has("tree") then {
      plotTree(
        "body",
        readFromString[MyD3TreeNode](url.searchParams.get("tree"))
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
      val _ = JSMain(simpleTreeNodeD3Tree)
    }
  }
}

private case class JSMain[F <: FugueFactory](
    d3Tree: D3Tree[F]
) {
  private var editors: Seq[TextReplication[d3Tree.factoryContext.type]] = List()

  /** In-memory links between editors, keyed by (lower index, higher index). Messages are delivered on sync only. */
  private var links: Map[(Int, Int), TextReplication.LocalLink] = Map.empty

  private def sync(i: Int, j: Int): Unit = {
    val key  = (math.min(i, j), math.max(i, j))
    val link = links.getOrElse(
      key, {
        val created = TextReplication.LocalLink(editors(key._1), editors(key._2))
        links = links.updated(key, created)
        created
      }
    )
    link.sync()
  }

  private val initialTexts = Seq(
    "Editor 0: edit here, then sync with editor 1 to merge the changes.",
    "Editor 1: this replica starts with its own text and can be edited independently."
  )

  private def installStyles(): Unit = {
    val style = document.createElement("style")
    style.innerHTML = """
      *,
      *::before,
      *::after {
        box-sizing: border-box;
      }
      body {
        margin: 0;
        background: #f1f5f9;
        color: #0f172a;
        font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
      }
      .text-rdt-app {
        max-width: 1200px;
        margin: 0 auto;
        padding: 2rem 1.5rem 3rem;
      }
      .text-rdt-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 1rem;
        flex-wrap: wrap;
      }
      .text-rdt-header h1 {
        margin: 0 0 0.35rem;
        font-size: 1.75rem;
      }
      .text-rdt-header p {
        margin: 0;
        max-width: 42rem;
        color: #64748b;
      }
      .text-rdt-nav {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem;
        margin-top: 0.75rem;
        font-size: 0.875rem;
      }
      .text-rdt-nav a {
        color: #4f46e5;
        text-decoration: none;
      }
      .text-rdt-nav a:hover {
        text-decoration: underline;
      }
      .editor-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));
        gap: 1.5rem;
        margin-top: 1.5rem;
      }
      .editor-panel {
        display: flex;
        flex-direction: column;
        gap: 0.75rem;
        min-width: 0;
        padding: 1rem;
        background: #fff;
        border: 1px solid #e2e8f0;
        border-radius: 14px;
        box-shadow: 0 1px 2px rgba(15, 23, 42, 0.06), 0 8px 24px -16px rgba(15, 23, 42, 0.25);
      }
      .editor-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.75rem;
      }
      .editor-header h3 {
        margin: 0;
        font-size: 1rem;
        font-weight: 600;
      }
      .editor-surface {
        height: 14rem;
        overflow: auto;
        padding: 0.75rem 0.9rem;
        background: #fff;
        border: 1px solid #cbd5e1;
        border-radius: 10px;
        transition: border-color 0.15s ease, box-shadow 0.15s ease;
      }
      .editor-surface:focus-within {
        border-color: #6366f1;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15);
      }
      .editor-surface .ProseMirror {
        min-height: 100%;
        outline: none;
        white-space: pre-wrap;
        overflow-wrap: anywhere;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 0.95rem;
        line-height: 1.6;
      }
      .editor-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
      }
      .editor-graph {
        overflow-x: auto;
      }
      .editor-graph:empty {
        display: none;
      }
      button {
        padding: 0.45rem 0.8rem;
        border: 1px solid #cbd5e1;
        border-radius: 8px;
        background: #f8fafc;
        color: #0f172a;
        font: inherit;
        cursor: pointer;
        transition: background 0.15s ease, border-color 0.15s ease;
      }
      button:hover {
        background: #eef2ff;
        border-color: #a5b4fc;
      }
      button.primary {
        background: #4f46e5;
        border-color: #4338ca;
        color: #fff;
      }
      button.primary:hover {
        background: #4338ca;
      }
      button.sync {
        padding: 0.35rem 0.65rem;
        font-size: 0.85rem;
      }
    """
    val _ = document.head.appendChild(style)
  }

  installStyles()

  org.scalajs.dom.document.body.innerHTML = """
  <div class="text-rdt-app">
    <header class="text-rdt-header">
      <div>
        <h1>Collaborative Text</h1>
        <p>Two independent replicas of a Fugue CRDT. Edit both, then use the sync buttons to merge them.</p>
        <nav class="text-rdt-nav">
          <a href="?simple">Simple</a>
          <a href="?complex">Complex</a>
          <a href="?simpleavl">Simple AVL</a>
          <a href="?complexavl">Complex AVL</a>
          <a href="?p2p">Peer-to-peer</a>
        </nav>
      </div>
      <button id="create-editor" class="primary">Create editor</button>
    </header>
    <div id="container" class="editor-grid"></div>
  </div>
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
        if element.classList.contains("sync") then {
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

          sync(dataReplica1, dataReplica2)
        }
      }
    )

  private def addEditor(initialText: String = ""): Unit = {
    val editorIndex = editors.length
    val newEditor   = document.createElement("div")
    newEditor.id = s"editor$editorIndex-parent"
    newEditor.classList.add("editor-panel")

    val header = document.createElement("div")
    header.classList.add("editor-header")
    val title = document.createElement("h3")
    title.innerText = s"Editor $editorIndex"
    val toggleGraph = document.createElement("button")
    toggleGraph.id = s"editor$editorIndex-toggle-graph"
    toggleGraph.innerText = "Toggle graphs"
    val _ = header.appendChild(title)
    val _ = header.appendChild(toggleGraph)

    val surface = document.createElement("div")
    surface.classList.add("editor-surface")
    surface.id = s"editor$editorIndex"

    val actions = document.createElement("div")
    actions.classList.add("editor-actions")
    actions.id = s"editor$editorIndex-actions"

    val graph = document.createElement("div")
    graph.classList.add("editor-graph")
    graph.id = s"editor$editorIndex-graph"
    val graphAvl = document.createElement("div")
    graphAvl.classList.add("editor-graph")
    graphAvl.id = s"editor$editorIndex-graph-avl"

    val _ = newEditor.appendChild(header)
    val _ = newEditor.appendChild(surface)
    val _ = newEditor.appendChild(actions)
    val _ = newEditor.appendChild(graph)
    val _ = newEditor.appendChild(graphAvl)

    for i <- editors.indices do {
      {
        val button = document.createElement("button")
        button.id = s"sync-editor$i-editor$editorIndex"
        button.classList.add("sync")
        button.textContent = s"Sync with $i"
        button.asInstanceOf[js.Dynamic].dataset.replica1 = i
        button.asInstanceOf[js.Dynamic].dataset.replica2 = editorIndex
        val _ = actions.appendChild(button)
      }
      {
        val button = document.createElement("button")
        button.id = s"sync-editor$editorIndex-editor$i"
        button.classList.add("sync")
        button.textContent = s"Sync with $editorIndex"
        button.asInstanceOf[js.Dynamic].dataset.replica1 = editorIndex
        button.asInstanceOf[js.Dynamic].dataset.replica2 = i
        val _ = document
          .querySelector(s"#editor$i-actions")
          .appendChild(button)
      }
    }

    val _ = org.scalajs.dom.document
      .querySelector("#container")
      .appendChild(newEditor)

    val editor = createEditor(s"$editorIndex", initialText)
    editors = editors.appended(editor)
  }

  private def createEditor(
      name: String,
      initialText: String
  ): TextReplication[d3Tree.factoryContext.type] = {
    val replicaState =
      new ReplicaState[d3Tree.factoryContext.type](
        name
      )(using d3Tree.factoryContext)

    val state = EditorState.create(editorStateConfig)

    val replica =
      Replica[d3Tree.factoryContext.type](replicaState)

    val prosemirror =
      ProseMirror(s"#editor$name", state, d3Tree, replica)

    replica.editor = prosemirror

    val replication = TextReplication(replica)
    prosemirror.onLocalChange = () => replication.publish()

    prosemirror.setText(initialText)

    replication
  }

  initialTexts.foreach(text => addEditor(text))
}
