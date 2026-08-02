package rdts.test.propertybased

import org.scalacheck.Test.Parameters
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import rdts.base.Lattice.syntax.merge
import rdts.base.{Lattice, LocalUid}

import scala.collection.mutable
import scala.util.Try

object StateBasedTestParameters {
  def update(param: Parameters): Parameters = param
    .withMinSize(30)
    .withMaxSize(200)
}

/** An API for stateful testing of ARDTs, removing a lot of the clutter of ScalaCheck's Commands API.
  * Users should use the trait `ACommand` for their commands which only needs an implementation of `nextLocalState` which is a function from a map of states to the next local state.
  * @tparam LocalState the type of the ARDT
  */
trait CommandsARDTs[LocalState: Lattice] extends Commands:
    override type State = Map[LocalUid, LocalState]
    override type Sut   = scala.collection.mutable.Map[LocalUid, LocalState]

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]): Boolean = true

    override def newSut(state: State): Sut = mutable.Map.from(state)

    override def destroySut(sut: Sut): Unit = sut.clear()

    override def initialPreCondition(state: State): Boolean = true

    def genId(state: State): Gen[LocalUid] = Gen.oneOf(state.keys)

    def genId2(state: State): Gen[(LocalUid, LocalUid)] =
        val ids = state.keys.toList
        for
            leftIndex <- Gen.choose(0, ids.length - 1)
            offset    <- Gen.choose(1, ids.length - 1)
            rightIndex = (leftIndex + offset) % ids.length
        yield (ids(leftIndex), ids(rightIndex))

    def genIdSubset(state: State): Gen[Set[LocalUid]] =
        val ids = state.keys.toList
        for
          amount <- Gen.choose(1, ids.size)
          elements <- Gen.pick(amount, ids)
        yield elements.toSet

    trait ACommand(id: LocalUid) extends Command:
        override type Result = State
        def nextLocalState(states: State): LocalState

        override def run(sut: Sut): Result =
            sut.update(id, nextLocalState(sut.toMap))
            sut.toMap

        override def nextState(state: State): State =
          state.updated(id, nextLocalState(state))

        override def preCondition(state: Map[LocalUid, LocalState]) = true

        override def postCondition(state: Map[LocalUid, LocalState], result: Try[Result]): Prop = result.isSuccess

    trait DuoCommand(actor: LocalUid, receiver: LocalUid) extends Command:
        override type Result = State

        def nextLocalStates(state: State): (LocalState, LocalState)

        override def run(sut: Sut): Result =
            val (left, right) = nextLocalStates(sut.toMap)
            sut.update(actor, left)
            sut.update(receiver, right)
            sut.toMap

        override def nextState(state: State): State =
            val (left, right) = nextLocalStates(state)
            state.updated(actor, left).updated(receiver, right)

        override def preCondition(state: Map[LocalUid, LocalState]) = true

        override def postCondition(state: Map[LocalUid, LocalState], result: Try[Result]): Prop = result.isSuccess

    trait BroadCastCommand(actor: LocalUid, receivers: Set[LocalUid]) extends Command:
        override type Result = State

        def delta(state: State): LocalState

        override def run(sut: Sut): Result =
            val d = delta(sut.toMap)
            sut.mapValuesInPlace((i, s) => if receivers.contains(i) then s.merge(d) else s)
            sut.toMap

        override def nextState(state: State): State =
            val d = delta(state)
            state.map((i, s) => if receivers.contains(i) then (i, s.merge(d)) else (i, s))

        override def preCondition(state: Map[LocalUid, LocalState]) = true

        override def postCondition(state: Map[LocalUid, LocalState], result: Try[Result]): Prop = result.isSuccess
