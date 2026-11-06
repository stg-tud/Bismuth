package text_rdt

import ujson.Str
import upickle.default.*

import scala.collection.mutable

enum ComplexAVLMessage {
  case Insert(
      rid: RID,
      counter: Int,
      offset: Int,
      var valueOrValues: StringBuilder,
      parentRid: RID | Null,
      parentCounter: Int,
      parentOffset: Int,
      side: Side
  ) extends ComplexAVLMessage()
  case Delete(rid: RID, counter: Int, var offset: Int, var count: Int = 1)
      extends ComplexAVLMessage()
}

object ComplexAVLMessage {

  given Rid: ReadWriter[RID | Null] =
    readwriter[ujson.Value].bimap[RID | Null](
      x =>
        x match {
          case null     => ujson.Null
          case rid: RID => ujson.Str(rid)
        },
      json => {
        json match {
          case Str(value)             => value
          case value: ujson.Null.type => null
          case other => ???
        }
      }
    )

  given fooReadWrite: ReadWriter[StringBuilder] =
    readwriter[ujson.Value]
      .bimap[StringBuilder](
        x => writeJs(x.toString()),
        json => StringBuilder(read[String](json))
      )

  given rw1[V: ReadWriter]: ReadWriter[ComplexAVLMessage.Insert] = macroRW
  given rw2[V: ReadWriter]: ReadWriter[ComplexAVLMessage.Delete] = macroRW

  given rw[V: ReadWriter]: ReadWriter[ComplexAVLMessage] = macroRW
}
