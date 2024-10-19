package text_rdt

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.collection.mutable

case class Convergence(revision: Int) {}

case class InternalMultiReplicaConvergenceTest[A](
    val factoryConstructor: String => A
)(using algorithm: CollaborativeTextEditingAlgorithm[A]) extends Commands {

  // the second parameter is just a unique id so uniquely identify equal convergences??
  type State = (Map[Int, Convergence], Int)

  type Sut = mutable.Map[Int, A]

  override def newSut(state: State): Sut = {
    assert(state._1.isEmpty)
    mutable.Map.empty
  }

  override def initialPreCondition(state: State): Boolean =
    state._1.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    val gens = List(
      genCreateReplica(state),
      genSyncReplicas(state),
      genInsert(state),
      genDelete(state)
    ).flatten
    Gen.choose(0, gens.size - 1).flatMap(i => gens(i))
  }

  def genCreateReplica(state: State): Option[Gen[CreateReplica]] = Some(for {
    replicaId <- Gen.choose(0, Int.MaxValue)
    if !state._1.keySet.contains(replicaId)
  } yield CreateReplica(replicaId))

  def genSyncReplicas(state: State): Option[Gen[SyncReplicas]] = if (
    state._1.size <= 1
  ) {
    None
  } else {
    Some(for {
      replicaIndex1 <- Gen.oneOf(state._1.keySet)
      replicaIndex2 <- Gen.oneOf(state._1.keySet diff Set(replicaIndex1))
    } yield {
      SyncReplicas(replicaIndex1, replicaIndex2)
    })
  }

  def genInsert(state: State): Option[Gen[Insert]] = if (state._1.isEmpty) {
    None
  } else {
    Some(for {
      replicaIndex <- Gen.oneOf(state._1.keySet)
      index <- Gen.chooseNum(0, Int.MaxValue)
      character <- Gen.asciiPrintableChar
    } yield Insert(replicaIndex, index, character))
  }

  def genDelete(state: State): Option[Gen[Delete]] = if (state._1.isEmpty) {
    None
  } else {
    Some(for {
      replicaIndex <- Gen.oneOf(state._1.keySet)
      index <- Gen.chooseNum(0, Int.MaxValue)
    } yield Delete(replicaIndex, index))
  }

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] =
    Gen.const((Map.empty, 0))

  override def destroySut(sut: Sut): Unit = {}

  abstract class BaseCommand extends SuccessCommand {
    type Result = Map[Int, String]

    override def postCondition(state: State, result: Result): Prop = {
      val grouped = nextState(state)._1.groupBy(e => e._2.revision)
      val groupsToCompare = grouped
        .map(elem => elem._2.toList.map(e => e._1))
        .filter(l => l.length > 1)
        .toList
      val prop = Prop.all(
        groupsToCompare.flatMap(group =>
          group
            .sliding(2)
            .map(ab =>
              Prop.=?(
                result(ab.head),
                result(ab(1))
              ) :| s"$group should all be equal"
            )
            .toSeq
        )*
      )
      prop
    }
  }

  case class CreateReplica(id: Int) extends BaseCommand {

    override def preCondition(state: State): Boolean = true

    override def run(sut: Sut): Result = {
      val replica = factoryConstructor(
        id.toString
      )
      val _ = sut.put(id, replica)
      sut.view.mapValues(_.text()).toMap
    }

    override def nextState(state: State): State = {
      (
        state._1 ++ Iterable(
          (id, Convergence(state._2 + 1))
        ),
        state._2 + 1
      )
    }
  }

  case class SyncReplicas(replicaIndex1: Int, replicaIndex2: Int)
      extends BaseCommand {

    override def preCondition(state: State): Boolean =
      state._1.contains(replicaIndex1) && state._1.contains(replicaIndex2)

    override def run(sut: Sut): Result = {
      val replica1 = sut(replicaIndex1)
      val replica2 = sut(replicaIndex2)

      if (sut.isInstanceOf[Replica[?]]) {
        assert(replica1.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == replica1.text())
        assert(replica2.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == replica2.text())
      }

      replica1.sync(replica2.asInstanceOf[replica1.type])

      if (sut.isInstanceOf[Replica[?]]) {
        assert(replica1.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == replica1.text(), s"${replica1.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString()} == ${replica1.text()}")
        assert(replica2.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == replica2.text(), s"${replica2.asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString()} == ${replica2.text()}")
      }

      sut.view.mapValues(_.text()).toMap
    }

    override def nextState(state: State): State = {
      val convergence = Convergence(
        state._2 + 1
      )
      (
        state._1
          .updated(replicaIndex1, convergence)
          .updated(replicaIndex2, convergence),
        state._2 + 1
      )
    }
  }

  case class Insert(replicaIndex: Int, index: Int, character: Char)
      extends BaseCommand {

    override def run(sut: Sut): Result = {
      val len =
        sut(replicaIndex).text().length()

      if (sut.isInstanceOf[Replica[?]]) {
        assert(sut(replicaIndex).asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == sut(replicaIndex).text())
      }

      sut(replicaIndex).insert(index % (len + 1), character)

      if (sut.isInstanceOf[Replica[?]]) {
        assert(sut(replicaIndex).asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == sut(replicaIndex).text())
      }

      sut.view.mapValues(_.text()).toMap
    }

    override def preCondition(state: State): Boolean =
      state._1.contains(replicaIndex)

    override def nextState(state: State): State = {
      (
        state._1.updated(
          replicaIndex,
          Convergence(state._2 + 1)
        ),
        state._2 + 1
      )
    }
  }

  case class Delete(replicaIndex: Int, index: Int) extends BaseCommand {

    override def run(sut: Sut): Result = {
      val len =
        sut(replicaIndex).text().length()

      if (sut.isInstanceOf[Replica[?]]) {
        assert(sut(replicaIndex).asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == sut(replicaIndex).text())
      }

      if (len > 0) {
        sut(replicaIndex).delete(index % len)
      }

      if (sut.isInstanceOf[Replica[?]]) {
        assert(sut(replicaIndex).asInstanceOf[Replica[?]].editor.asInstanceOf[StringEditory].data.toString() == sut(replicaIndex).text())
      }

      sut.view.mapValues(_.text()).toMap
    }

    override def preCondition(state: State): Boolean =
      state._1.contains(replicaIndex)

    override def nextState(state: State): State = {
      (
        state._1.updated(
          replicaIndex,
          Convergence(state._2 + 1)
        ),
        state._2 + 1
      )
    }
  }
}
