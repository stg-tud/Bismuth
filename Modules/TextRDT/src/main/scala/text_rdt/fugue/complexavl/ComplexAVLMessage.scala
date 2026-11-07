package text_rdt.fugue.complexavl

import text_rdt.fugue.{*}
import text_rdt.{*}

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
