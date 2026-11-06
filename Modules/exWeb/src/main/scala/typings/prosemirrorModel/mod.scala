package typings.prosemirrorModel

import typings.orderedmap.mod.OrderedMap
import typings.prosemirrorModel.prosemirrorModelStrings.full
import org.scalablytyped.runtime.{StObject, StringDictionary}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object mod {

  type Attrs         = StringDictionary[Any]
  type DOMOutputSpec = String | Array[String | Any]

  @JSImport("prosemirror-model", "Node")
  @js.native
  open class Node extends js.Object {
    def textBetween(from: Double, to: Double): String = js.native
  }

  @JSImport("prosemirror-model", "Fragment")
  @js.native
  open class Fragment extends js.Object {
    def textBetween(from: Double, to: Double): String = js.native
    def size: Double                                  = js.native
  }

  @JSImport("prosemirror-model", "Slice")
  @js.native
  open class Slice extends js.Object {
    val content: Fragment = js.native
    def size: Double      = js.native
  }

  @js.native
  trait NodeSpec extends js.Object
  object NodeSpec {
    inline def apply(): NodeSpec = js.Dynamic.literal().asInstanceOf[NodeSpec]

    implicit open class MutableBuilder[Self <: NodeSpec](val x: Self) extends AnyVal {
      inline def setContent(value: String): Self   = StObject.set(x, "content", value.asInstanceOf[js.Any])
      inline def setMarks(value: String): Self     = StObject.set(x, "marks", value.asInstanceOf[js.Any])
      inline def setCode(value: Boolean): Self     = StObject.set(x, "code", value.asInstanceOf[js.Any])
      inline def setDefining(value: Boolean): Self = StObject.set(x, "defining", value.asInstanceOf[js.Any])
      inline def setParseDOM(value: js.Array[TagParseRule]): Self =
        StObject.set(x, "parseDOM", value.asInstanceOf[js.Any])
      inline def setToDOM(value: Node => DOMOutputSpec): Self = StObject.set(x, "toDOM", js.Any.fromFunction1(value))
    }
  }

  @js.native
  trait TagParseRule extends js.Object {
    var tag: String
  }
  object TagParseRule {
    inline def apply(tag: String): TagParseRule = {
      val __obj = js.Dynamic.literal(tag = tag.asInstanceOf[js.Any])
      __obj.asInstanceOf[TagParseRule]
    }

    implicit open class MutableBuilder[Self <: TagParseRule](val x: Self) extends AnyVal {
      inline def setPreserveWhitespace(value: Boolean | full): Self =
        StObject.set(x, "preserveWhitespace", value.asInstanceOf[js.Any])
    }
  }

  @js.native
  trait SchemaSpec extends js.Object
  object SchemaSpec {
    inline def apply(nodes: OrderedMap[NodeSpec]): SchemaSpec = {
      val __obj = js.Dynamic.literal(nodes = nodes.asInstanceOf[js.Any])
      __obj.asInstanceOf[SchemaSpec]
    }
  }

  @JSImport("prosemirror-model", "Schema")
  @js.native
  open class Schema[Nodes, Marks](spec: SchemaSpec) extends js.Object
}
