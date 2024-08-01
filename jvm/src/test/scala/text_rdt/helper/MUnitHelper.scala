package text_rdt.helper

import com.microsoft.playwright.{Page, Playwright}
import munit.Fixture

import java.util.concurrent.ConcurrentLinkedQueue
import com.microsoft.playwright.Tracing
import java.nio.file.Paths
import com.microsoft.playwright.Browser
import java.util.UUID
import com.microsoft.playwright.BrowserType

class WebDriverFixture
    extends Fixture[ConcurrentLinkedQueue[Browser]]("webdriver") {
  val playwright = Playwright.create.nn

  private val drivers: ConcurrentLinkedQueue[Browser] =
    ConcurrentLinkedQueue()

  private def createWebDriver(): Browser = {
    val browser =
      playwright.chromium.nn
        .launch(new BrowserType.LaunchOptions().setHeadless(true))
        .nn
    browser
  }

  def getOrCreateWebDriver(): Page = {
    val driverOrNull: Browser | Null = drivers.poll()
    val optionDriver = if (driverOrNull eq null) {
      None
    } else {
      Some(driverOrNull)
    }
    val browser = optionDriver.getOrElse(createWebDriver())
    val context = browser.newContext().nn;
    context.newPage.nn
  }

  def giveBack(webDriver: Page, uuid: UUID): Unit = {
    val browser = webDriver.context().nn.browser().nn
    webDriver.context().nn.close()
    assert(drivers.offer(browser))
  }

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {
    drivers.forEach(driver => {
      driver.nn.close()
    })
    playwright.close()
  }

  override def apply(): ConcurrentLinkedQueue[Browser] = drivers
}
