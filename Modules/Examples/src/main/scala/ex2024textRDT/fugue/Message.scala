package ex2024textRDT.fugue

import ex2024textRDT.*

enum Message[ID] {
  case Insert(
      id: ID,
      value: Char,
      parent: ID | Null,
      side: Side
  )                   extends Message[ID]()
  case Delete(id: ID) extends Message[ID]()
}
