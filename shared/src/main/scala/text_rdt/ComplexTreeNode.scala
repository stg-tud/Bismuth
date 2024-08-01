package text_rdt

import text_rdt.Helper.myAssert

import scala.collection.mutable

final case class ComplexTreeNode(
    id: SimpleID | Null,
    var offset: Int,
    var values: Either[mutable.ArrayBuffer[Char], Int],
    var allowAppend: Boolean,
    var parent: ComplexTreeNodeSingle | Null,
    side: Side,
    var leftChildrenBuffer: mutable.ArrayBuffer[
      ComplexTreeNodeSingle
    ],
    var rightChildrenBuffer: mutable.ArrayBuffer[
      ComplexTreeNodeSingle
    ]
) {
  inline def invariant(): Unit = {
    myAssert(
      leftChildrenBuffer.forall(c =>
        c.complexTreeNode.side == Side.Left && c.complexTreeNode.parent.nn.complexTreeNode
          .eq(this)
      )
    )
    myAssert(
      rightChildrenBuffer.forall(c =>
        c.complexTreeNode.side == Side.Right && c.complexTreeNode.parent.nn.complexTreeNode
          .eq(this)
      )
    )
    myAssert(
      parent match {
        case null => true
        case parent =>
          parent.complexTreeNode.leftChildrenBuffer
            .map(_.complexTreeNode)
            .contains(this) || parent.complexTreeNode.rightChildrenBuffer
            .map(_.complexTreeNode)
            .contains(this)
      }
    )
  }

  override def toString: String = pprintNonAVL.apply(this).plainText

  def valueSize(): Int = {
    values match {
      case Left(value)  => value.size
      case Right(value) => value
    }
  }

  def visibleValueSize(): Int = {
    values match {
      case Left(value)  => value.size
      case Right(value) => 0
    }
  }
}

final case class ComplexTreeNodeSingle(
    complexTreeNode: ComplexTreeNode,
    index: Int
) {
  myAssert(
    (index - complexTreeNode.offset) < complexTreeNode.valueSize(),
    s"$index - ${complexTreeNode.offset} < ${complexTreeNode.valueSize()}"
  )

  myAssert(
    0 <= index - complexTreeNode.offset,
    s"0 <= $index - ${complexTreeNode.offset}"
  )

  def valueIndex(): Int = {
    index - complexTreeNode.offset
  }

  def visibleValueIndex(): Int = {
    complexTreeNode.values match {
      case Left(value)  => index - complexTreeNode.offset
      case Right(value) => 0
    }
  }
}

object ComplexTreeNodeSingle {
  given canEqual: CanEqual[ComplexTreeNodeSingle, ComplexTreeNodeSingle] =
    CanEqual.derived

  given complexTreeNodeSingle: TreeNodey[ComplexTreeNodeSingle] with {
    override type ID = ComplexID

    extension (treeNode: ComplexTreeNodeSingle) {

      override def parent(): ComplexTreeNodeSingle | Null = {
        if (treeNode.valueIndex() == 0) {
          treeNode.complexTreeNode.parent
        } else {
          ComplexTreeNodeSingle(treeNode.complexTreeNode, treeNode.index - 1)
        }
      }

      override def parentId(): ComplexID | Null = {
        if (treeNode.valueIndex() == 0) {
          treeNode.complexTreeNode.parent.nn.id()
        } else {
          ComplexID(
            treeNode.complexTreeNode.id.nn.rid,
            treeNode.complexTreeNode.id.nn.counter,
            treeNode.index - 1
          )
        }
      }

      override def id(): ComplexID | Null =
        treeNode.complexTreeNode.id match {
          case null => null
          case value =>
            ComplexID(
              value.rid,
              value.counter,
              treeNode.index
            )
        }

      override def value(): Char | Null =
        treeNode.complexTreeNode.values match {
          case Left(values) =>
            values(treeNode.valueIndex())
          case Right(value) =>
            myAssert(treeNode.valueIndex() < value)
            null
        }

      override def leftChildren(): Iterator[ComplexTreeNodeSingle] = {
        if (treeNode.valueIndex() == 0) {
          treeNode.complexTreeNode.leftChildrenBuffer.iterator
        } else {
          Iterator.empty
        }
      }

      override def rightChildren(): Iterator[ComplexTreeNodeSingle] = {
        if (treeNode.valueIndex() + 1 < treeNode.complexTreeNode.valueSize()) {
          Iterator(
            ComplexTreeNodeSingle(
              treeNode.complexTreeNode,
              treeNode.index + 1
            )
          )
        } else {
          treeNode.complexTreeNode.rightChildrenBuffer.iterator
        }
      }

      override def side(): Side = treeNode.complexTreeNode.side
    }
  }
}
