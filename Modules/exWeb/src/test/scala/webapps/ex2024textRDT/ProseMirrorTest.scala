package webapps.ex2024textRDT

import text_rdt.*
import text_rdt.given

import munit.FunSuite
import org.scalajs.dom
import typings.prosemirrorState.mod.EditorState

import scala.scalajs.js
import scala.util.Random

/** Tests the ProseMirror integration headlessly: the editor, the translation of ProseMirror
  * transactions into replica operations, and the propagation of remote operations back into the
  * editor. Everything runs on node with jsdom, no browser required.
  */
class ProseMirrorTest extends FunSuite {

  override def beforeAll(): Unit =
    try JsdomEnv.install()
    catch {
      case e: js.JavaScriptException =>
        println("JS STACK:\n" + e.exception.asInstanceOf[js.Dynamic].stack)
        throw e
    }

  private def setupDom(count: Int): Unit =
    dom.document.body.innerHTML = (0 until count)
      .map(i =>
        s"""<div id="editor$i"></div>
           |<div id="editor$i-graph"></div>
           |<div id="editor$i-graph-avl"></div>
           |<button id="editor$i-toggle-graph"></button>""".stripMargin
      )
      .mkString("\n")

  private def newReplicaAndEditor[F <: FugueFactory](
      i: Int,
      d3Tree: D3Tree[F]
  ): (Replica[d3Tree.factoryContext.type], ProseMirror[F]) = {
    val replicaState = ReplicaState[d3Tree.factoryContext.type](s"$i")(using
      d3Tree.factoryContext
    )
    val replica = Replica(replicaState)
    val editor  = ProseMirror(
      s"#editor$i",
      EditorState.create(editorStateConfig),
      d3Tree,
      replica
    )
    replica.editor = editor
    (replica, editor)
  }

  private def domText(i: Int): String =
    dom.document.querySelector(s"#editor$i").textContent

  private def insert(editor: ProseMirror[?], index: Int, char: Char): Unit =
    editor.dispatchTransaction(
      editor.state.tr.insertText(char.toString, index)
    )

  private def remove(editor: ProseMirror[?], index: Int): Unit =
    editor.dispatchTransaction(
      editor.state.tr.delete(index, index + 1)
    )

  /** Randomly inserts and deletes through ProseMirror and checks that the replica and the DOM stay
    * in sync with a plain string model.
    */
  private def singleReplicaTest[F <: FugueFactory](
      d3Tree: D3Tree[F],
      seed: Int
  ): Unit = {
    setupDom(1)
    val (replica, editor) = newReplicaAndEditor(0, d3Tree)
    val random            = new Random(seed)
    var text              = ""

    for _ <- 0 until 200 do {
      if text.isEmpty || random.nextBoolean() then {
        val index = random.nextInt(text.length + 1)
        val char  = ('a' + random.nextInt(26)).toChar
        insert(editor, index, char)
        text = text.patch(index, char.toString, 0)
      } else {
        val index = random.nextInt(text.length)
        remove(editor, index)
        text = text.patch(index, "", 1)
      }
      assertEquals(replica.text(), text)
      assertEquals(domText(0), text)
    }
  }

  /** Two independent editors, randomly editing and syncing, must converge and both DOM views must
    * follow their replica.
    */
  private def multiReplicaTest[F <: FugueFactory](
      d3Tree: D3Tree[F],
      seed: Int
  ): Unit = {
    setupDom(2)
    val (replica0, editor0) = newReplicaAndEditor(0, d3Tree)
    val (replica1, editor1) = newReplicaAndEditor(1, d3Tree)
    val random              = new Random(seed)

    val replication0 = TextReplication(replica0)
    val replication1 = TextReplication(replica1)
    editor0.onLocalChange = () => replication0.publish()
    editor1.onLocalChange = () => replication1.publish()
    val link = TextReplication.LocalLink(replication0, replication1)

    for _ <- 0 until 60 do {
      val (editor, size) =
        if random.nextBoolean() then (editor0, replica0.text().length)
        else (editor1, replica1.text().length)
      if size == 0 || random.nextBoolean() then {
        val char = ('a' + random.nextInt(26)).toChar
        insert(editor, random.nextInt(size + 1), char)
      } else {
        remove(editor, random.nextInt(size))
      }

      assertEquals(domText(0), replica0.text())
      assertEquals(domText(1), replica1.text())

      link.sync()
      assertEquals(replica0.text(), replica1.text())
      assertEquals(domText(0), replica0.text())
      assertEquals(domText(1), replica1.text())
    }
  }

  test("simple: insert and delete through ProseMirror transactions update the replica") {
    singleReplicaTest(simpleTreeNodeD3Tree, 42)
  }

  test("complex: insert and delete through ProseMirror transactions update the replica") {
    singleReplicaTest(complexTreeNodeD3Tree, 42)
  }

  test("simpleavl: insert and delete through ProseMirror transactions update the replica") {
    singleReplicaTest(simpleAVLTreeNodeD3Tree, 42)
  }

  test("complexavl: insert and delete through ProseMirror transactions update the replica") {
    singleReplicaTest(complexAVLTreeNodeD3Tree, 42)
  }

  test("simple: two editors converge after syncing") {
    multiReplicaTest(simpleTreeNodeD3Tree, 7)
  }

  test("complex: two editors converge after syncing") {
    multiReplicaTest(complexTreeNodeD3Tree, 7)
  }

  test("simpleavl: two editors converge after syncing") {
    multiReplicaTest(simpleAVLTreeNodeD3Tree, 7)
  }

  test("complexavl: two editors converge after syncing") {
    multiReplicaTest(complexAVLTreeNodeD3Tree, 7)
  }

  test("simple: setting the initial text updates the replica and DOM") {
    setupDom(1)
    val (replica, editor) = newReplicaAndEditor(0, simpleTreeNodeD3Tree)
    editor.setText("hello world")
    assertEquals(replica.text(), "hello world")
    assertEquals(domText(0), "hello world")
  }

  test("JSMain boots with two pre-filled editors for the simple algorithm") {
    dom.window.history.replaceState(null.asInstanceOf[js.Any], "", "/?simple")
    setupDom(0)
    JSMain.main()
    assert(dom.document.querySelector("#editor0-parent") != null)
    assert(dom.document.querySelector("#editor1-parent") != null)
    assert(domText(0).nonEmpty)
    assert(domText(1).nonEmpty)

    dom.document
      .querySelector("#create-editor")
      .asInstanceOf[js.Dynamic]
      .click()
    assert(dom.document.querySelector("#editor2-parent") != null)
  }
}
