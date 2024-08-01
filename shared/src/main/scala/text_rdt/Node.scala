package text_rdt

final case class Node(
    id: SimpleID | Null,
    value: Char | Null,
    parent: SimpleTreeNode | Null,
    side: Side
) {}
