package typings.prosemirrorView

import org.scalablytyped.runtime.StObject
import org.scalajs.dom.Element
import typings.prosemirrorState.mod.{EditorState, Transaction}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object mod {

  type DOMNode = Element

  @JSImport("prosemirror-view", "EditorView")
  @js.native
  open class EditorView(place: DOMNode, props: DirectEditorProps) extends js.Object {
    def updateState(state: EditorState): Unit = js.native
  }

  @js.native
  trait DirectEditorProps extends js.Object
  object DirectEditorProps {
    inline def apply(state: EditorState): DirectEditorProps = {
      val __obj = js.Dynamic.literal(state = state.asInstanceOf[js.Any])
      __obj.asInstanceOf[DirectEditorProps]
    }

    implicit open class MutableBuilder[Self <: DirectEditorProps](val x: Self) extends AnyVal {
      inline def setDispatchTransaction(value: Transaction => Unit): Self =
        StObject.set(x, "dispatchTransaction", js.Any.fromFunction1(value))
    }
  }
}
