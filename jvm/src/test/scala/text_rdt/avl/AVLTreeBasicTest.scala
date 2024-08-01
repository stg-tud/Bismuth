package text_rdt.avl

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import text_rdt.Side

import scala.collection.mutable

final case class AVLTreeBasicTest() extends Commands {

  type State = Seq[String]

  type Sut = AVLTree[String]

  override def newSut(state: State): Sut = {
    assert(state.isEmpty)
    AVLTree(null)
  }

  override def initialPreCondition(state: State): Boolean = state.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    genInsert(state)
  }

  def genInsert(state: State): Gen[Insert] = for {
    offsetInArray <- Gen.chooseNum(0, Int.MaxValue)
    value <- Gen.stringOfN(2, Gen.asciiPrintableChar)
  } yield {
    Insert(offsetInArray, value)
  }

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] = Gen.const(Seq.empty)

  override def destroySut(sut: Sut): Unit = {}

  case class Insert(offsetInArray: Int, value: String) extends SuccessCommand {
    type Result = (String, String)

    override def run(sut: Sut): Result = {
      var log = ""
      val values = sut.values()
      val index =
        offsetInArray % (values.length + 1)
      val offset =
        if (index == values.length) {
          values
            .slice(0, index)
            .map(_.length())
            .sum
        } else {
          values
            .slice(0, index)
            .map(_.length())
            .sum
        }
      if (index == values.length && offset != 0) {
        val nodeToInsertAt = sut.nodeAtIndex(offset - 1).nn._1
        log += s"inserting to the right of $nodeToInsertAt"
        sut.insert(
          nodeToInsertAt,
          AVLTreeNode(_ => value),
          Side.Right
        )
      } else {
        val nodeToInsertAt = sut.nodeAtIndex(offset).nn._1
        log += s"inserting to the left of $nodeToInsertAt"
        sut.insert(
          nodeToInsertAt,
          AVLTreeNode(_ => value),
          Side.Left
        )
      }
      (sut.values().mkString, log + s" index $index offset $offset")
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Result): Prop = {
      Prop.=?(nextState(state).mkString, result._1)
    }

    override def nextState(state: State): State = {
      val result = mutable.ListBuffer
        .from(state)
      result.insert(offsetInArray % (state.length + 1), value)
      result.toSeq
    }
  }
}
