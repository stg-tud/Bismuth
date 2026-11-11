package ex2024textRDT.fugue.simple

import ex2024textRDT.*
import ex2024textRDT.fugue.*

final case class Node(
    id: SimpleID | Null,
    value: Char | Null,
    parent: SimpleTreeNode | Null,
    side: Side
) {}
