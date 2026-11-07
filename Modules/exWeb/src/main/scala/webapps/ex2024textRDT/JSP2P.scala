package webapps.ex2024textRDT

import org.scalajs.dom.{Element, document}
import text_rdt.*
import text_rdt.fugue.ComplexAVLFugueFactory.complexAVLFugueFactory
import text_rdt.given
import text_rdt.fugue.{*, given}
import text_rdt.debug.{*, given}
import typings.prosemirrorState.mod.EditorState
import webapps.WebRTCConnectionView

import java.util.UUID

/** A single editor replicated with peers over WebRTC (connection setup via [[WebRTCConnectionView]]). */
private case class JSP2P() {

  org.scalajs.dom.document.body.innerHTML = """
  <div id="container" style="display: grid; grid-template-columns: 1fr 1fr;">
  </div>
  <div id="peers"></div>
  """

  val replicaId: String = UUID.randomUUID().nn.toString()

  val newEditor: Element = document.createElement("div")
  newEditor.id = s"editor${replicaId}-parent"
  val child1: Element = document.createElement("h3")
  child1.innerText = "Editor"
  val child2: Element = document.createElement("div")
  child2.classList.add("my-border")
  child2.id = s"editor${replicaId}"
  val child3: Element = document.createElement("pre")
  child3.id = s"editor${replicaId}-info"
  val child4: Element = document.createElement("button")
  child4.id = s"editor${replicaId}-toggle-graph"
  child4.innerText = "Toggle graphs"
  val child5: Element = document.createElement("div")
  child5.id = s"editor${replicaId}-graph"
  val child6: Element = document.createElement("div")
  child6.id = s"editor${replicaId}-graph-avl"
  val _ = newEditor.appendChild(child1)
  val _ = newEditor.appendChild(child2)
  val _ = newEditor.appendChild(child3)
  val _ = newEditor.appendChild(child4)
  val _ = newEditor.appendChild(child5)
  val _ = newEditor.appendChild(child6)

  val _ = org.scalajs.dom.document
    .querySelector("#container")
    .appendChild(newEditor)

  val replicaState =
    new ReplicaState[complexAVLTreeNodeD3Tree.factoryContext.type](
      replicaId
    )(using complexAVLTreeNodeD3Tree.factoryContext)

  val replica: Replica[complexAVLTreeNodeD3Tree.factoryContext.type] =
    Replica[complexAVLTreeNodeD3Tree.factoryContext.type](
      replicaState,
    )

  val prosemirror: ProseMirror[complexAVLFugueFactory.type] =
    ProseMirror(
      s"#editor${replicaId}",
      EditorState.create(editorStateConfig),
      complexAVLTreeNodeD3Tree,
      replica
    )

  replica.editor = prosemirror

  val replication: TextReplication[complexAVLTreeNodeD3Tree.factoryContext.type] = TextReplication(replica)
  prosemirror.onLocalChange = () => replication.publish()

  val _ = document
    .querySelector("#peers")
    .appendChild(WebRTCConnectionView(replication.io).example().render)
}
