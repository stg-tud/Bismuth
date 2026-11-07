package text_rdt.fugue.simple

import text_rdt.fugue.{*, given}
import text_rdt.{*, given}

final case class Node(
    id: SimpleID | Null,
    value: Char | Null,
    parent: SimpleTreeNode | Null,
    side: Side
) {}
