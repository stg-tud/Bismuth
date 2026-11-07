package webapps.ex2024textRDT

import scala.scalajs.js.annotation.JSExportTopLevel

/** Entry point for the text-rdt demo, wired up through `text-rdt.html` in the web bundle. */
object TextRDT {

  @JSExportTopLevel("TextRDT")
  def run(): Unit =
    JSMain.main()
}
