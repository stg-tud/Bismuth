package ex2024textRDT.fugue.complexavl

import ex2024textRDT.fugue.{*}
import ex2024textRDT.{*}

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
