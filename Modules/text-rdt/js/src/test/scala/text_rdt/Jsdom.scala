package text_rdt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Minimal facade for the bits of the jsdom npm package the tests use. */
@JSImport("jsdom", "JSDOM")
@js.native
class JSDOM(html: String, options: js.Object) extends js.Object {
  val window: js.Dynamic = js.native
}

object JsdomEnv {

  private def setGlobal(name: String, value: js.Any): Unit =
    js.Dynamic.global.globalThis.asInstanceOf[js.Dynamic].updateDynamic(name)(value)

  /** Installs a jsdom window/document as the global browser environment. Must be called before any
    * javascript library that captures window/document at import time is loaded.
    */
  def install(): Unit = {
    val dom = new JSDOM(
      "<!doctype html><html><body></body></html>",
      js.Dynamic.literal(pretendToBeVisual = true, url = "http://localhost/")
    )
    val window = dom.window
    setGlobal("window", window)
    setGlobal("document", window.document)
    setGlobal("MutationObserver", window.MutationObserver)
    setGlobal("DOMParser", window.DOMParser)
    setGlobal("getSelection", window.getSelection.bind(window))
    setGlobal("requestAnimationFrame", window.requestAnimationFrame.bind(window))
  }
}
