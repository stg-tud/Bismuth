package text_rdt

import text_rdt.avl.AVLTreeNode

import scala.collection.mutable
import upickle.default._

case class MyD3TreeNode(
    value: String,
    leftChildren: Array[MyD3TreeNode],
    rightChildren: Array[MyD3TreeNode],
    var x: Double = 0,
    var y: Double = 0,
    var width: Double = 0,
    var height: Double = 0
) derives ReadWriter

trait D3Tree[F <: FugueFactory](using val factoryContext: F) {

  extension (node: factoryContext.N) {
    def buildTree(): MyD3TreeNode
  }
}

given simpleTreeNodeD3Tree: D3Tree[SimpleFugueFactory.simpleFugueFactory.type](
  using SimpleFugueFactory.simpleFugueFactory
) with {

  extension (treeNode: SimpleTreeNode) {
    private def tt(): String = {
      (treeNode.value() match {
        case null  => "⌫"
        case value => value.toString
      }) + " " + (treeNode
        .id() match {
        case null  => "root"
        case value => s"${value.rid}#${value.counter}"
      })
    }

    private def buildTree(
        prefix: String
    ): MyD3TreeNode = {
      MyD3TreeNode(
        treeNode.tt(),
        treeNode
          .leftChildren()
          .map(
            _.buildTree(prefix + treeNode.tt() + "/")
          )
          .toArray,
        treeNode
          .rightChildren()
          .map(
            _.buildTree(prefix + treeNode.tt() + "/")
          )
          .toArray
      )
    }

    def buildTree(): MyD3TreeNode = {
      buildTree("")
    }
  }
}

given complexTreeNodeD3Tree
    : D3Tree[ComplexFugueFactory.complexFugueFactory.type](using
  ComplexFugueFactory.complexFugueFactory
) with {

  private def tt(treeNode: ComplexTreeNode): String = {
    s"${treeNode.values match {
        case Left(value)  => value.mkString
        case Right(value) => "⌫".repeat(value)
      }} ${treeNode.id match {
        case null  => "root"
        case value => s"${value.rid}#${value.counter}"
      }}"
  }

  private def buildTree(
      prefix: String,
      treeNode: ComplexTreeNode
  ): MyD3TreeNode = {
    MyD3TreeNode(
      tt(treeNode),
      treeNode.leftChildrenBuffer.iterator
        .map((c: ComplexTreeNodeSingle) =>
          complexTreeNodeD3Tree
            .buildTree(prefix + tt(treeNode) + "/", c.complexTreeNode)
        )
        .toArray,
      treeNode.rightChildrenBuffer.iterator
        .map((c: ComplexTreeNodeSingle) =>
          complexTreeNodeD3Tree
            .buildTree(prefix + tt(treeNode) + "/", c.complexTreeNode)
        )
        .toArray
    )
  }

  extension (treeNode: ComplexTreeNodeSingle) {
    def buildTree(): MyD3TreeNode = {
      complexTreeNodeD3Tree.buildTree("", treeNode.complexTreeNode)
    }
  }
}

given simpleAVLTreeNodeD3Tree
    : D3Tree[SimpleAVLFugueFactory.simpleAVLFugueFactory.type](using
  SimpleAVLFugueFactory.simpleAVLFugueFactory
) with {

  extension (treeNode: AVLTreeNode[SimpleAVLTreeNode]) {
    private def tt(): String = {
      (treeNode.value() match {
        case null  => "⌫"
        case value => value.toString
      }) + " " + (treeNode
        .id() match {
        case null  => "root"
        case value => s"${value.rid}#${value.counter}"
      })
    }

    private def buildTree(
        prefix: String
    ): MyD3TreeNode = {
      MyD3TreeNode(
        treeNode.tt(),
        treeNode
          .leftChildren()
          .iterator
          .map(
            _.buildTree(prefix + treeNode.tt() + "/")
          )
          .toArray,
        treeNode
          .rightChildren()
          .iterator
          .map(
            _.buildTree(prefix + treeNode.tt() + "/")
          )
          .toArray
      )
    }

    def buildTree(): MyD3TreeNode = {
      buildTree("")
    }
  }
}

given complexAVLTreeNodeD3Tree
    : D3Tree[ComplexAVLFugueFactory.complexAVLFugueFactory.type](using
  ComplexAVLFugueFactory.complexAVLFugueFactory
) with {

  private def tt(
      treeNode: AVLTreeNode[ComplexAVLTreeNode]
  ): String = {
    s"${treeNode.value._values match {
        case null  => "⌫".repeat(treeNode.value.to - treeNode.value.offset + 1)
        case value => treeNode.value.values.mkString
      }} ${treeNode.value.rid match {
        case null => "root"
        case rid =>
          s"${rid}#${treeNode.value.counter}.${treeNode.value.offset}-${treeNode.value.to}"
      }}"
  }

  private def buildTree(
      prefix: String,
      treeNode: AVLTreeNode[ComplexAVLTreeNode]
  ): MyD3TreeNode = {
    MyD3TreeNode(
      tt(treeNode),
      treeNode.value.leftChildrenBuffer match {
        case null => Array.empty
        case singleChild: AVLTreeNode[ComplexAVLTreeNode] =>
          Array(
            complexAVLTreeNodeD3Tree
              .buildTree(prefix + tt(treeNode) + "/", singleChild)
          )
        case childrenBuffer: mutable.SortedSet[AVLTreeNode[
              ComplexAVLTreeNode
            ]] =>
          childrenBuffer.iterator
            .map((c: AVLTreeNode[ComplexAVLTreeNode]) =>
              complexAVLTreeNodeD3Tree
                .buildTree(prefix + tt(treeNode) + "/", c)
            )
            .toArray
      },
      treeNode.value.rightChildrenBuffer match {
        case null => Array.empty
        case singleChild: AVLTreeNode[ComplexAVLTreeNode] =>
          Array(
            complexAVLTreeNodeD3Tree
              .buildTree(prefix + tt(treeNode) + "/", singleChild)
          )
        case childrenBuffer: mutable.SortedSet[AVLTreeNode[
              ComplexAVLTreeNode
            ]] =>
          childrenBuffer.iterator
            .map((c: AVLTreeNode[ComplexAVLTreeNode]) =>
              complexAVLTreeNodeD3Tree
                .buildTree(prefix + tt(treeNode) + "/", c)
            )
            .toArray
      }
    )
  }

  extension (treeNode: ComplexAVLTreeNodeSingle) {
    def buildTree(): MyD3TreeNode = {
      complexAVLTreeNodeD3Tree.buildTree("", treeNode.complexTreeNode)
    }
  }
}
