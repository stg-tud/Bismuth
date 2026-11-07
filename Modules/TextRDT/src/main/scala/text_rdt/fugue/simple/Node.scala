package text_rdt.fugue.simple

import text_rdt.fugue.{*}
import text_rdt.{*}

final case class Node(
    id: SimpleID | Null,
    value: Char | Null,
    parent: SimpleTreeNode | Null,
    side: Side
) {}
