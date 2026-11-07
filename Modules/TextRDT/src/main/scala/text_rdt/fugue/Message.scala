package text_rdt.fugue

import text_rdt.{*, given}

enum Message[ID] {
  case Insert(
      id: ID,
      value: Char,
      parent: ID | Null,
      side: Side
  )                   extends Message[ID]()
  case Delete(id: ID) extends Message[ID]()
}
