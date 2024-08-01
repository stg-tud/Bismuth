package text_rdt

import com.microsoft.playwright.{Locator, Page}
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import text_rdt.helper.WebDriverFixture
import java.util.UUID

case class SutSingleType(
    driver: Page,
    element: Locator,
    traceUuid: UUID
) {}

case class BrowserSingleReplicaInsertDeleteTest(
    webDriverFixture: WebDriverFixture,
    algorithm: String
) extends Commands {

  type State = String

  type Sut = SutSingleType

  override def newSut(state: State): Sut = {
    assert(state.isEmpty)
    val driver = webDriverFixture.getOrCreateWebDriver()
    val traceUuid = UUID.randomUUID().nn
    try {
      driver.navigate(s"http://localhost:5173/?$algorithm")
      val createEditor = driver.locator("#create-editor").nn
      createEditor.click()
      val editor = driver.locator("#editor0 div").nn
      SutSingleType(driver, editor, traceUuid)
    } catch
      case e => {
        webDriverFixture.giveBack(driver, traceUuid); throw e
      }
  }

  override def initialPreCondition(state: State): Boolean = state.isEmpty

  override def genCommand(state: State): Gen[Command] = if (state.isEmpty) {
    genInsert(state)
  } else {
    Gen.oneOf(genInsert(state), genDelete(state))
  }

  def genInsert(state: State): Gen[Insert] = for {
    index <- Gen.chooseNum(0, state.length())
    character <- Gen.asciiPrintableChar
  } yield Insert(index, character)

  def genDelete(state: State): Gen[Delete] = for {
    index <- Gen.chooseNum(0, state.length() - 1)
  } yield Delete(index)

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def genInitialState: Gen[State] = Gen.const("")

  override def destroySut(sut: Sut): Unit = {
    webDriverFixture.giveBack(sut.driver, sut.traceUuid)
  }

  abstract class BaseCommand extends SuccessCommand {
    type Result = (String, UUID)

    override def postCondition(state: State, result: Result): Prop = {
      Prop
        .=?(nextState(state), result._1)
    }
  }

  case class Insert(index: Int, character: Char) extends BaseCommand {

    override def run(sut: Sut): Result = {
      val oldText =
        sut.element.textContent().nn
      sut.element
        .fill(StringBuilder(oldText).insert(index, character).toString)
      (sut.element.textContent().nn, sut.traceUuid)
    }

    override def preCondition(state: State): Boolean = index <= state.length()

    override def nextState(state: State): State = {
      val builder = StringBuilder(state)
      builder.insert(index, character)
      builder.result()
    }
  }

  case class Delete(index: Int) extends BaseCommand {

    override def run(sut: Sut): Result = {
      val oldText =
        sut.element.textContent().nn
      val toFill = StringBuilder(oldText).deleteCharAt(index).toString
      if (toFill.isEmpty()) {
        sut.element.fill("\n")
      } else {
        sut.element
          .fill(toFill)
      }
      (sut.element.textContent().nn, sut.traceUuid)
    }

    override def preCondition(state: State): Boolean = {
      index < state.length()
    }

    override def nextState(state: State): State = {
      val builder = StringBuilder(state)
      builder.deleteCharAt(index)
      builder.result()
    }
  }
}
