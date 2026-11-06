package text_rdt.avl2

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import text_rdt.Side

case class Wrapper(var value: AVL2Tree[Unit])

final case class AVL2TreeBasicTest() extends Commands {

  type State = Seq[Int]

  type Sut = Wrapper

  override def newSut(state: State): Sut = {
    assert(state.isEmpty)
    Wrapper(AVL2Tree(null, ()))
  }

  override def initialPreCondition(state: State): Boolean = state.isEmpty

  override def genCommand(state: State): Gen[Command] = if (state.isEmpty) {
    genInsert(state)
  } else {
    Gen.oneOf(genInsert(state), genSplit(state))
  }

  def genInsert(state: State): Gen[Insert] = if (state.isEmpty) {
    for {
      value <- Gen.choose(0, Int.MaxValue)
      side <- Gen.oneOf(Side.Left, Side.Right)
    } yield {
      Insert(side, null, value)
    }
  } else {
    for {
      value <- Gen.choose(0, Int.MaxValue)
      if !state.contains(value)
      side <- Gen.oneOf(Side.Left, Side.Right)
    } yield {
      side match {
        case text_rdt.Side.Left =>
          state.filter(_ > value).minOption match {
            case None =>
              state.filter(_ < value).maxOption match {
                case None       => ???
                case Some(base) => Insert(Side.Right, base, value)
              }
            case Some(base) => Insert(Side.Left, base, value)
          }
        case text_rdt.Side.Right =>
          state.filter(_ < value).maxOption match {
            case None =>
              state.filter(_ > value).minOption match {
                case None       => ???
                case Some(base) => Insert(Side.Left, base, value)
              }
            case Some(base) => Insert(Side.Right, base, value)
          }
      }
    }
  }

  def genSplit(state: State): Gen[Split] =
    for {
      value <- Gen.oneOf(state)
      side <- Gen.oneOf(Side.Left, Side.Right)
    } yield Split(side, value)

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] = Gen.const(Seq.empty)

  override def destroySut(sut: Sut): Unit = {}

  case class Insert(side: Side, base: Int | Null, value: Int)
      extends SuccessCommand {

    type Result = Seq[Int]

    override def preCondition(state: State): Boolean = true

    override def run(sut: Sut): Result = {
      base match {
        case null => null
        case value =>
          sut.value.collectNodes().find(_.value == base) match {
            case None =>
            case Some(baseNode) =>
              sut.value.insert(baseNode, AVL2TreeNode(value, sut.value), side)
          }
      }
      sut.value.values()
    }

    override def postCondition(state: State, result: Result): Prop = {
      Prop.=?(result, nextState(state).sorted)
    }

    override def nextState(state: State): State = {
      if (state.contains(base)) {
        state.appended(value)
      } else {
        state
      }
    }
  }

  case class Split(side: Side, value: Int) extends SuccessCommand {
    type Result = (Seq[Int], Seq[Int])

    override def preCondition(state: State): Boolean = true

    override def nextState(state: State): State = {
      side match {
        case text_rdt.Side.Left  => state.filter(_ <= value)
        case text_rdt.Side.Right => state.filter(_ > value)
      }
    }

    override def run(sut: Sut): Result = {
      val leftTree = AVL2Tree[Unit](null, ())
      val (left, right) = sut.value.split(value, leftTree)

      side match {
        case text_rdt.Side.Left  => sut.value = left
        case text_rdt.Side.Right => sut.value = right
      }

      (left.values(), right.values())
    }

    override def postCondition(
        state: State,
        result: Result
    ): Prop = {
      Prop.=?(result._1, state.filter(_ <= value).sorted) && Prop.=?(
        result._2,
        state.filter(_ > value).sorted
      )
    }
  }
}
