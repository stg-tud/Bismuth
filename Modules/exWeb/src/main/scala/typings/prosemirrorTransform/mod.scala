package typings.prosemirrorTransform

import typings.prosemirrorModel.mod.{Node, Slice}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object mod {

  @JSImport("prosemirror-transform", "Transform")
  @js.native
  open class Transform protected () extends js.Object {
    def delete(from: Double, to: Double): this.type = js.native
    val docs: js.Array[Node]                        = js.native
    val steps: js.Array[Step]                       = js.native
  }

  @JSImport("prosemirror-transform", "Step")
  @js.native
  open class Step protected () extends js.Object {
    def toJSON(): js.Any = js.native
  }

  @JSImport("prosemirror-transform", "ReplaceStep")
  @js.native
  open class ReplaceStep_ protected () extends Step {
    val from: Double = js.native
    val to: Double   = js.native
    val slice: Slice = js.native
  }
}
