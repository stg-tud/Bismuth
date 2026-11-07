package typings.prosemirrorState

import org.scalablytyped.runtime.StObject
import typings.prosemirrorModel.mod.Schema
import typings.prosemirrorTransform.mod.Transform
import typings.prosemirrorView.mod.EditorView

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}

object mod {

  type Command = js.Function3[
    /* state */ EditorState,
    /* dispatch */ js.UndefOr[js.Function1[ /* tr */ Transaction, Unit]],
    /* view */ js.UndefOr[EditorView],
    Boolean
  ]

  @JSImport("prosemirror-state", "EditorState")
  @js.native
  open class EditorState protected () extends js.Object {
    def tr: Transaction = js.native
    @JSName("apply")
    def applyTransaction(tr: Transaction): EditorState = js.native
  }
  object EditorState {
    @JSImport("prosemirror-state", "EditorState")
    @js.native
    val ^ : js.Any = js.native

    inline def create(config: EditorStateConfig): EditorState =
      ^.asInstanceOf[js.Dynamic].applyDynamic("create")(config.asInstanceOf[js.Any]).asInstanceOf[EditorState]
  }

  @JSImport("prosemirror-state", "Transaction")
  @js.native
  open class Transaction protected () extends Transform {
    def insertText(text: String): this.type                           = js.native
    def insertText(text: String, from: Double): this.type             = js.native
    def insertText(text: String, from: Double, to: Double): this.type = js.native
  }

  @JSImport("prosemirror-state", "Plugin")
  @js.native
  open class Plugin[State] extends js.Object

  @js.native
  trait EditorStateConfig extends js.Object
  object EditorStateConfig {
    inline def apply(): EditorStateConfig = js.Dynamic.literal().asInstanceOf[EditorStateConfig]

    implicit open class MutableBuilder[Self <: EditorStateConfig](val x: Self) extends AnyVal {
      inline def setPluginsVarargs(value: Plugin[Any]*): Self = StObject.set(x, "plugins", js.Array(value*))
      inline def setSchema(value: Schema[Any, Any]): Self     = StObject.set(x, "schema", value.asInstanceOf[js.Any])
    }
  }
}
