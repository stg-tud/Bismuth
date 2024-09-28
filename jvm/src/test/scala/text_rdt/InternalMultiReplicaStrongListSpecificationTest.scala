package text_rdt

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.collection.mutable

// https://dl.acm.org/doi/pdf/10.1145/2933057.2933090
// If I understand the paper it does not have the strongest possible property.
// In any case my idea would be to verify that if once order has been decided, reordering is not allowed at all (so even if multiple replicas have only partial knowledge of the text, their merge is not allowed to reorder). I fullfilling this is not possible, counter examples may be interesting. I think though that for good user behavior this would be a nice property.

// I think in this test we leave the state just empty and track everything in sut and result

// a second test could store a "global order knowledge" in the state and also which character is known by which peer (deleted and non-deleted) and then try to check everything? Ordering concurrent edits then would be hard though? Wouldn't this kind of create the fugue tree?

case class InternalMultiReplicaStrongListSpecificationTest[F <: FugueFactory]()(
    using val factoryContext: F
) extends Commands {

  // used replica ids
  type State = Set[Int]

  type Sut = mutable.Map[Int, Replica[F]]

  override def newSut(state: State): Sut = {
    assert(state.isEmpty)
    mutable.Map.empty
  }

  override def initialPreCondition(state: State): Boolean = state.isEmpty

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
    if !state.contains(replicaId)
  } yield CreateReplica(replicaId))

  def genSyncReplicas(state: State): Option[Gen[SyncReplicas]] = if (
    state.size <= 1
  ) {
    None
  } else {
    Some(for {
      replicaIndex1 <- Gen.oneOf(state)
      replicaIndex2 <- Gen.oneOf(state diff Set(replicaIndex1))
    } yield {
      SyncReplicas(replicaIndex1, replicaIndex2)
    })
  }

  def genInsert(state: State): Option[Gen[Insert]] = if (state.isEmpty) {
    None
  } else {
    Some(for {
      replicaIndex <- Gen.oneOf(state)
      index <- Gen.chooseNum(0, Int.MaxValue)
      character <- Gen.asciiPrintableChar
    } yield Insert(replicaIndex, index, character))
  }

  def genDelete(state: State): Option[Gen[Delete]] = if (state.isEmpty) {
    None
  } else {
    Some(for {
      replicaIndex <- Gen.oneOf(state)
      index <- Gen.chooseNum(0, Int.MaxValue)
    } yield Delete(replicaIndex, index))
  }

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] =
    Gen.const(Set.empty)

  override def destroySut(sut: Sut): Unit = {}

  abstract class BaseCommand extends SuccessCommand {
    // map of text before and map of text after
    // either left is character, either right is deleted character
    type Result = (Map[Int, Vector[Either[Char, Char]]], Map[Int, Vector[Either[Char, Char]]])
  }

  case class CreateReplica(replicaIndex: Int) extends BaseCommand {

    override def preCondition(state: State): Boolean = true

    override def run(sut: Sut): Result = {
      val before = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap

      val replicaState = ReplicaState(
        replicaIndex.toString
      )(using factoryContext)
      val replica = Replica(replicaState, NoopEditory())
      val _ = sut.put(replicaIndex, replica)

      val after = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap
      (before, after)
    }

    override def nextState(state: State): State = {
      state ++ Set(replicaIndex)
    }

    override def postCondition(state: Set[Int], result: Result): Prop = {
      Prop.all(
        (List(Prop.=?(state, result._1.keySet), Prop.=?(state ++ Set(replicaIndex), result._2.keySet))
          ++
          result._1.map((key, value) => Prop.=?(value, result._2(key))).toList
        )*
      )
    }
  }

  case class SyncReplicas(replicaIndex1: Int, replicaIndex2: Int)
      extends BaseCommand {

    override def preCondition(state: State): Boolean =
      state.contains(replicaIndex1) && state.contains(replicaIndex2)

    override def run(sut: Sut): Result = {
      val before = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap

      val replica1 = sut(replicaIndex1)
      val replica2 = sut(replicaIndex2)

      replica1.sync(replica2.asInstanceOf[replica1.type])

      val after = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap
      (before, after)
    }

    override def nextState(state: State): State = {
      state
    }

    override def postCondition(state: Set[Int], result: Result): Prop = {
      // check that text before of replica 1 is a subset of text after
      // check that text before of replica 2 is a subset of text after
      // oh no deletions could change this, right

      // maybe check that order of what is still there  is still correct? can we even do this? I don't think so

     // val newText = StringBuilder(result._1(replicaIndex)).insert(index % (result._1(replicaIndex).length + 1), character).toString()
      // TODO check that before can produce text after
      Prop.all(
        (List(Prop.=?(state, result._1.keySet), Prop.=?(state, result._2.keySet), Prop.=?(result._2(replicaIndex1), result._2(replicaIndex2)))
          ++
          (result._1.view.filterKeys(key => key != replicaIndex1 && key != replicaIndex2)).map((key, value) => Prop.=?(value, result._2(key))).toList
          )*
      )
    }
  }

  case class Insert(replicaIndex: Int, index: Int, character: Char)
      extends BaseCommand {

    override def run(sut: Sut): Result = {
      val before = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap

      val state = sut(replicaIndex).state
      val len = state.factoryContext.textWithDeleted(state.factory)().filter(_.isLeft).size
      state.insert(index % (len + 1), character)

      val after = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap
      (before, after)
    }

    override def preCondition(state: State): Boolean =
      state.contains(replicaIndex)

    override def nextState(state: State): State = {
       state
    }

    override def postCondition(state: Set[Int], result: Result): Prop = {
      println(s"result $result")
      val newText = result._1(replicaIndex).to(mutable.ArrayBuffer)


      // TODO FIXME this whole shit does not work if there are multiple deleted characters after each other

      val indexModulo = index % (result._1(replicaIndex).count(_.isLeft) + 1)
      var currentIndex = 0
      var collectionIndex = newText.indexWhere(value => {
        if (value.isLeft) {
          if (currentIndex == indexModulo) {
            true
          } else {
            currentIndex += 1
            false
          }
        } else {
          false
        }
      })
      if (collectionIndex == -1) {
        collectionIndex = newText.size - 1
      }
      newText.insert(collectionIndex, Left(character))
      Prop.all(
        (List(Prop.=?(state, result._1.keySet), Prop.=?(state, result._2.keySet), Prop.=?(newText, result._2(replicaIndex)))
          ++
          (result._1.view.filterKeys(key => key != replicaIndex)).map((key, value) => Prop.=?(value, result._2(key))).toList
          )*
      )
    }
  }

  case class Delete(replicaIndex: Int, index: Int) extends BaseCommand {

    override def run(sut: Sut): Result = {
      val before = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap

      val state = sut(replicaIndex).state
      val len =
        state.factoryContext.textWithDeleted(state.factory)().filter(_.isLeft).size
      if (len > 0) {
        state.delete(index % len)
      }

      val after = sut.view.mapValues(r => r.state.factoryContext.textWithDeleted(r.state.factory)()).toMap
      (before, after)
    }

    override def preCondition(state: State): Boolean =
      state.contains(replicaIndex)

    override def nextState(state: State): State = {
      state
    }

    override def postCondition(state: Set[Int], result: Result): Prop = {
      val newText = result._1(replicaIndex).to(mutable.ArrayBuffer)
      if (result._1(replicaIndex).count(_.isLeft) != 0) {
        // skip deleted
        val indexModulo = index % result._1(replicaIndex).count(_.isLeft)
        var currentIndex = 0
        val collectionIndex = newText.indexWhere(value => {
          if (value.isLeft) {
            if (currentIndex == indexModulo) {
              true
            } else {
              currentIndex += 1
              false
            }
          } else {
            false
          }
        })
        newText(collectionIndex) = Right(newText(collectionIndex).left.get)
      }
      Prop.all(
        (List(Prop.=?(state, result._1.keySet), Prop.=?(state, result._2.keySet), Prop.=?(newText, result._2(replicaIndex)))
          ++
          (result._1.view.filterKeys(key => key != replicaIndex)).map((key, value) => Prop.=?(value, result._2(key))).toList
          ) *
      )
    }
  }
}
