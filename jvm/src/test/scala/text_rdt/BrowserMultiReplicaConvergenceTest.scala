package text_rdt

import com.microsoft.playwright.{Locator, Page}
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import text_rdt.helper.WebDriverFixture

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import java.util.UUID

case class SutMultiType(
    driver: Page,
    elements: ListBuffer[Locator],
    traceUuid: UUID
) {}

case class BrowserMultiReplicaConvergenceTest(
    webDriverFixture: WebDriverFixture,
    algorithm: String
) extends Commands {

  private var revision = 0

  def nextRevision: Int = {
    revision += 1
    revision
  }

  type State = List[Convergence]

  type Sut = SutMultiType

  override def newSut(state: State): Sut = {
    assert(state.isEmpty)
    val driver = webDriverFixture.getOrCreateWebDriver()
    val traceUuid = UUID.randomUUID().nn
    try {
      driver.navigate(s"http://localhost:5173/?$algorithm")
      SutMultiType(driver, ListBuffer.empty, traceUuid)
    } catch
      case e => {
        webDriverFixture.giveBack(driver, traceUuid); throw e
      }
  }

  override def initialPreCondition(state: State): Boolean =
    state.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    Gen.frequency(
      1 -> genCreateReplica(state),
      (if (state.size <= 1) 0 else 4) -> genSyncReplicas(state),
      (if (state.isEmpty) 0 else 8) -> genInsert(state),
      (if (state.isEmpty) 0 else 8) -> genDelete(state)
    )
  }

  def genCreateReplica(state: State): Gen[CreateReplica] =
    Gen.const(CreateReplica())

  def genSyncReplicas(state: State): Gen[SyncReplicas] = if (state.size <= 1) {
    SyncReplicas(Int.MaxValue, Int.MaxValue)
  } else {
    for {
      replicaIndex1 <- Gen.choose(0, state.size - 1)
      replicaIndex2 <- Gen.choose(0, state.size - 2)
    } yield {
      if (replicaIndex1 == replicaIndex2) {
        SyncReplicas(replicaIndex1, state.size - 1)
      } else {
        SyncReplicas(replicaIndex1, replicaIndex2)
      }
    }
  }

  def genInsert(state: State): Gen[Insert] = if (state.isEmpty) {
    Insert(Int.MaxValue, Int.MaxValue, ' ')
  } else {
    for {
      replicaIndex <- Gen.choose(0, state.size - 1)
      index <- Gen.chooseNum(0, Int.MaxValue)
      character <- Gen.asciiPrintableChar
    } yield Insert(replicaIndex, index, character)
  }

  def genDelete(state: State): Gen[Delete] = if (state.isEmpty) {
    Delete(Int.MaxValue, Int.MaxValue)
  } else {
    for {
      replicaIndex <- Gen.choose(0, state.size - 1)
      index <- Gen.chooseNum(0, Int.MaxValue)
    } yield Delete(replicaIndex, index)
  }

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] =
    Gen.const(List.empty)

  override def destroySut(sut: Sut): Unit = {
    webDriverFixture.giveBack(sut._1, sut.traceUuid)
  }

  abstract class BaseCommand extends SuccessCommand {
    type Result = (List[String], UUID)

    override def postCondition(state: State, result: Result): Prop = {
      val grouped = nextState(state).zip(result._1).groupBy(e => e._1.revision)
      val groupsToCompare = grouped
        .map(elem => elem._2.map(e => e._2))
        .filter(l => l.length > 1)
        .toList
      Prop.all(
        groupsToCompare.flatMap(group =>
          group
            .sliding(2)
            .map(ab => Prop.=?(ab.head, ab(1)))
            .toSeq
        )*
      )
    }
  }

  case class CreateReplica() extends BaseCommand {
    type Result = (List[String], UUID)

    override def preCondition(state: State): Boolean = true

    override def run(sut: Sut): Result = {
      val createEditor = sut._1.locator("#create-editor").nn
      createEditor.click()
      val editor =
        sut._1.locator(s"#editor${sut._2.length} div").nn
      val _ = sut._2.append(editor)
      (sut._2.map(_.textContent().nn).toList, sut.traceUuid)
    }

    override def nextState(state: State): State = {
      state.appended(
        Convergence(nextRevision)
      )
    }
  }

  case class SyncReplicas(replicaIndex1: Int, replicaIndex2: Int)
      extends BaseCommand {
    type Result = (List[String], UUID)

    override def preCondition(state: State): Boolean =
      replicaIndex1 < state.size && replicaIndex2 < state.size && replicaIndex1 != replicaIndex2

    override def run(sut: Sut): Result = {
      val syncButton =
        sut._1
          .locator(
            s"#sync-editor$replicaIndex1-editor$replicaIndex2"
          )
          .nn
      syncButton.click()
      (sut._2.map(_.textContent().nn).toList, sut.traceUuid)
    }

    override def nextState(state: State): State = {
      val convergence = Convergence(
        nextRevision
      )
      state
        .updated(replicaIndex1, convergence)
        .updated(replicaIndex2, convergence)
    }
  }

  case class Insert(replicaIndex: Int, index: Int, character: Char)
      extends BaseCommand {
    type Result = (List[String], UUID)

    override def run(sut: Sut): Result = {
      val oldText =
        sut._2(replicaIndex).textContent().nn
      val fixedIndex = index % (oldText.size + 1)

      sut
        .elements(replicaIndex)
        .fill(StringBuilder(oldText).insert(fixedIndex, character).toString)

      (sut._2.map(_.textContent().nn).toList, sut.traceUuid)
    }

    override def preCondition(state: State): Boolean =
      replicaIndex < state.size

    override def nextState(state: State): State = {
      state.updated(
        replicaIndex,
        Convergence(nextRevision)
      )
    }
  }

  case class Delete(replicaIndex: Int, index: Int) extends BaseCommand {
    type Result = (List[String], UUID)

    override def run(sut: Sut): Result = {
      val oldText =
        sut._2(replicaIndex).textContent().nn
      if (oldText.length() > 0) {
        val fixedIndex = index % oldText.length()

        val toFill = StringBuilder(oldText).deleteCharAt(fixedIndex).toString
        if (toFill.isEmpty()) {
          sut.elements(replicaIndex).fill("\n")
        } else {
          sut
            .elements(replicaIndex)
            .fill(toFill)
        }
      }
      (sut._2.map(_.textContent().nn).toList, sut.traceUuid)
    }

    override def preCondition(state: State): Boolean =
      replicaIndex < state.size

    override def nextState(state: State): State = {
      state.updated(
        replicaIndex,
        Convergence(nextRevision)
      )
    }
  }
}
