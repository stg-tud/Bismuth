package text_rdt

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.collection.mutable

case class MyResult(result: Seq[Seq[(String, String)]]) {
  override def toString: String =
    "\n" + result
      .map(r => r.map(v => v._2 + ": " + v._1).mkString("\n"))
      .mkString("\n----------------\n")
}

case class InternalFugueTreeEqualityTest(
    factoryConstructors: Array[(() => FugueFactory)]
) extends Commands {

  type State =
    List[String]

  type Sut = Vector[mutable.Map[String, Replica[?]]]

  override def newSut(state: State): Sut = {
    Vector.fill(factoryConstructors.length)(mutable.Map.empty)
  }

  override def initialPreCondition(state: State): Boolean = state.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    Gen.frequency(
      1 -> genCreateReplica(state),
      (if (state.length <= 1) 0 else 4) -> genSyncReplicas(state),
      (if (state.isEmpty) 0 else 8) -> genInsert(state),
      (if (state.isEmpty) 0 else 8) -> genDelete(state)
    )
  }

  def genCreateReplica(state: State): Gen[CreateReplica] = for {
    replicaId <- Gen.stringOfN(10, Gen.asciiPrintableChar)
  } yield CreateReplica(replicaId)

  def genSyncReplicas(state: State): Gen[SyncReplicas] = if (
    state.length <= 1
  ) {
    SyncReplicas("", "")
  } else {
    for {
      replica1 <- Gen.oneOf(state)
      replica2 <- Gen.oneOf(state diff List(replica1))
    } yield {
      SyncReplicas(replica1, replica2)
    }
  }

  def genInsert(state: State): Gen[Insert] = if (state.isEmpty) {
    Insert("", Int.MaxValue, ' ')
  } else {
    for {
      replica <- Gen.oneOf(state)
      index <- Gen.chooseNum(0, Int.MaxValue)
      character <- Gen.asciiPrintableChar
    } yield Insert(replica, index, character)
  }

  def genDelete(state: State): Gen[Delete] = if (state.isEmpty) {
    Delete("", Int.MaxValue)
  } else {
    for {
      replica <- Gen.oneOf(state)
      index <- Gen.chooseNum(0, Int.MaxValue)
    } yield Delete(replica, index)
  }

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] =
    Gen.const(List.empty)

  override def destroySut(sut: Sut): Unit = {}

  abstract class BaseCommand extends SuccessCommand {

    type Result = MyResult

    override def postCondition(state: State, result: Result): Prop = {
      val prop = Prop.all(
        result.result.head.indices.flatMap(replicaIndex => {
          factoryConstructors.indices
            .sliding(2)
            .map(ab =>
              Prop.=?(
                result.result(ab(0))(replicaIndex),
                result.result(ab(1))(replicaIndex)
              )
            )
            .toSeq
        })*
      )
      prop
    }
  }

  case class CreateReplica(replicaId: String) extends BaseCommand {

    override def preCondition(state: State): Boolean = true

    override def run(sut: Sut): Result = {
      MyResult(
        sut
          .zip(factoryConstructors)
          .map((replicas, factoryConstructor) => {
            val replicaState = ReplicaState(
              replicaId
            )(using factoryConstructor())
            val replica = Replica(replicaState, NoopEditory())
            val _ = replicas.put(replicaId, replica)
            replicas.values.map(v => (v.tree(), s"create $replicaId")).toSeq
          })
      )
    }

    def nextState(state: State): State = state ++ List(replicaId)
  }

  case class SyncReplicas(replicaId1: String, replicaId2: String)
      extends BaseCommand {

    override def preCondition(state: State): Boolean =
      state.contains(replicaId1) && state.contains(replicaId2)

    override def run(sut: Sut): Result = {
      MyResult(
        sut
          .map(replicas => {
            val replica1 = replicas(replicaId1)
            val replica2 = replicas(replicaId2)

            replica1.sync(
              replica2.asInstanceOf[replica1.type]
            )
            replicas.values
              .map(v => (v.tree(), s"sync $replicaId1 $replicaId2"))
              .toSeq
          })
      )
    }

    def nextState(state: State): State = state
  }

  case class Insert(replica: String, index: Int, character: Char)
      extends BaseCommand {

    override def run(sut: Sut): Result = {
      MyResult(
        sut
          .map(replicas => {
            val len =
              replicas(replica).text().length()
            replicas(replica).state.insert(index % (len + 1), character)
            replicas.values
              .map(v =>
                (v.tree(), s"insert $replica ${index % (len + 1)} $character")
              )
              .toSeq
          })
      )
    }

    override def preCondition(state: State): Boolean = state.contains(replica)

    def nextState(state: State): State = state
  }

  case class Delete(replica: String, index: Int) extends BaseCommand {

    override def run(sut: Sut): Result = {
      MyResult(
        sut
          .map(replicas => {
            val len =
              replicas(replica).text().length()
            if (len > 0) {
              replicas(replica).state.delete(index % len)
            }
            replicas.values
              .map(v =>
                (
                  v.tree(),
                  if (len > 0) { s"delete $replica index ${index % len}" }
                  else { s"don't delete $replica empty" }
                )
              )
              .toSeq
          })
      )
    }

    override def preCondition(state: State): Boolean = state.contains(replica)

    def nextState(state: State): State = state
  }
}
