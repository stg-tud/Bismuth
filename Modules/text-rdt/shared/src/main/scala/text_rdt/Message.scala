package text_rdt

enum Message[ID] {
  case Insert(
      id: ID,
      value: Char,
      parent: ID | Null,
      side: Side
  ) extends Message[ID]()
  case Delete(id: ID) extends Message[ID]()
}
