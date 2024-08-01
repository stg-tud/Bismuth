package text_rdt

import scala.annotation.tailrec

trait TreeNodey[N] {
  type ID

  extension (node: N) {
    def parent(): N | Null

    def parentId(): ID | Null

    def id(): ID | Null

    def value(): Char | Null

    def firstLeftChild(): N | Null = {
      leftChildren().nextOption().orNull
    }

    def leftChildren(): Iterator[N]

    def firstRightChild(): N | Null = {
      rightChildren().nextOption().orNull
    }

    def rightChildren(): Iterator[N]

    def side(): Side

    def leftmostDescendant(): N = {
      tailrecLeftmostDecendant()
    }

    @tailrec
    private final def tailrecLeftmostDecendant(): N = {
      node.firstLeftChild() match {
        case null  => node
        case value => value.tailrecLeftmostDecendant()
      }
    }

    def treeify(): pprint.Tree = pprint.Tree
      .Apply(
        "TreeNodey",
        Iterator(
          pprint.Tree
            .KeyValue(
              "value",
              pprint.treeify(node.value(), false, true)
            ),
          pprint.Tree.Apply(
            "leftChildren",
            node.leftChildren().map(n => n.treeify())
          ),
          pprint.Tree.Apply(
            "rightChildren",
            node.rightChildren().map(n => n.treeify())
          )
        )
      )
  }
}
