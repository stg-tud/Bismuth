package example

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedList
import rdts.syntax.DeltaBuffer

sealed trait Component
case class Button(text: String)                 extends Component
case class Label(name: String)                  extends Component
case class Container(children: List[Component]) extends Component

class RecursiveCompositionTest extends munit.FunSuite {

  given codec: JsonValueCodec[Component] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val exampleComposition: Component = Container(
    List(
      Label("Do you Accept?"),
      Container(
        List(
          Button("Yes"),
          Button("Ok")
        )
      )
    )
  )

  test("composition is serializable") {
    val bytes        = writeToArray(exampleComposition)
    val roundtripped = readFromArray[Component](bytes)
    assertEquals(roundtripped, exampleComposition)
  }

  test("replicatable") {
    // for technical reasons, some replicated data types need to be stored inside some containers that store their metadata
    // in this case, DeltaBuffer keeps track of updates and combines updates, while Dotted keeps track of causality
    val rlist = DeltaBuffer(ReplicatedList.empty[Component])

    // some operations on replicated data types require a unique ID per replica, here we just generate one randomly that is then used in the call to `append` below
    given myId: rdts.base.LocalUid = LocalUid.gen()

    // okay, this is cheating, this just adds complex components to a replicated list, and would not be sufficient for fine grained editing of replicated UIs … however, it should be sufficient for a very simple usecase
    rlist.mod(_.append(exampleComposition))
  }

}
