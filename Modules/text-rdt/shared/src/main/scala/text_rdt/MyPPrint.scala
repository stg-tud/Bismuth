package text_rdt

import pprint.PPrinter
import text_rdt.avl.AVLTreeNode
import text_rdt.avl2.AVL2TreeNode

lazy val pprintNonAVL: PPrinter =
  pprint.copy(
    additionalHandlers = {
      case value: AVLTreeNode[?] =>
        pprintNonAVL.treeify(value.value, false, true)
      case value: ComplexAVLTreeNode =>
        pprint.Tree.Apply(
          "ComplexAVLTreeNode",
          Iterator(
            pprint.Tree
              .KeyValue("id", pprintNonAVL.treeify(value.rid, false, true)),
            pprint.Tree
              .KeyValue(
                "counter",
                pprintNonAVL.treeify(value.counter, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "side",
                pprintNonAVL.treeify(value.side, false, true)
              ),
            pprint.Tree.KeyValue(
              "offset",
              pprintNonAVL.treeify(value.offset, false, true)
            ),
            pprint.Tree.KeyValue(
              "to",
              pprintNonAVL.treeify(value.to, false, true)
            ),
            pprint.Tree
              .KeyValue(
                "values",
                pprintNonAVL.treeify(
                  value._values,
                  false,
                  true
                )
              ),
            pprint.Tree
              .KeyValue(
                "leftChildrenBuffer",
                pprintNonAVL.treeify(value.leftChildrenBuffer, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "rightChildrenBuffer",
                pprintNonAVL.treeify(value.rightChildrenBuffer, false, true)
              )
          )
        )
      case value: SimpleAVLTreeNode =>
        pprint.Tree.Apply(
          "SimpleAVLTreeNode",
          Iterator(
            pprint.Tree
              .KeyValue("id", pprintNonAVL.treeify(value.id, false, true)),
            pprint.Tree
              .KeyValue(
                "side",
                pprintNonAVL.treeify(value.side, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.value, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "leftChildrenBuffer",
                pprintNonAVL.treeify(value.leftChildrenBuffer, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "rightChildrenBuffer",
                pprintNonAVL.treeify(value.rightChildrenBuffer, false, true)
              )
          )
        )
      case value: ComplexAVLTreeNodeSingle =>
        pprint.Tree.Apply(
          "ComplexAVLTreeNodeSingle",
          Iterator(
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.complexTreeNode, false, true)
              ),
            pprint.Tree
              .KeyValue("index", pprintNonAVL.treeify(value.index, false, true))
          )
        )
      case value: ComplexTreeNode =>
        pprint.Tree.Apply(
          "ComplexTreeNode",
          Iterator(
            pprint.Tree
              .KeyValue(
                "id",
                pprintNonAVL.treeify(value.id, false, true)
              ),
            pprint.Tree
              .KeyValue("offset", pprint.treeify(value.offset, false, true)),
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.values, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "side",
                pprintNonAVL.treeify(value.side, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "leftChildrenBuffer",
                pprintNonAVL.treeify(value.leftChildrenBuffer, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "rightChildrenBuffer",
                pprintNonAVL.treeify(value.rightChildrenBuffer, false, true)
              )
          )
        )
      case value: Node =>
        pprint.Tree.Apply(
          "Node",
          Iterator(
            pprint.Tree
              .KeyValue(
                "id",
                pprintNonAVL.treeify(value.id, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.value, false, true)
              ),
            pprint.Tree
              .KeyValue(
                "side",
                pprintNonAVL.treeify(value.side, false, true)
              )
          )
        )
    }
  )

lazy val pprintAVL: PPrinter =
  pprint.copy(
    additionalHandlers = {
      case value: AVLTreeNode[?] =>
        pprint.Tree.Apply(
          "AVLTreeNode",
          Iterator(
            pprint.Tree
              .KeyValue("left", pprintAVL.treeify(value.left, false, true)),
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.value, false, true)
              ),
            pprint.Tree
              .KeyValue("right", pprintAVL.treeify(value.right, false, true))
          )
        )
      case value: AVL2TreeNode[?] =>
        pprint.Tree.Apply(
          "AVL2TreeNode",
          Iterator(
            pprint.Tree
              .KeyValue("left", pprintAVL.treeify(value.left, false, true)),
            pprint.Tree
              .KeyValue(
                "value",
                pprintNonAVL.treeify(value.value, false, true)
              ),
            pprint.Tree
              .KeyValue("right", pprintAVL.treeify(value.right, false, true))
          )
        )
    }
  )
