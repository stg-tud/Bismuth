package ex2026webview

import io.avaje.webview.Webview as AvajeWebview

import java.nio.file.Path

/** Desktop (JVM/FFM) webview launcher backed directly by avaje-webview.
  *
  * Renders an exWeb page in a native window. Accepts either a URL (e.g. a local
  * vite dev server: `http://localhost:5173/todolist.html`) or a path to a local
  * html file.
  */
object Webview {
  def main(args: Array[String]): Unit = {
    if args.isEmpty then
        println(s"requires a url or a path to an html file as a first argument")
        return

    val target = args.head
    val uri =
      if target.startsWith("http://") || target.startsWith("https://") then target
      else Path.of(target).toUri.toString

    val webview = AvajeWebview
      .builder()
      .title("Bismuth Web Apps")
      .enableDeveloperTools(true) // right-click > Inspect
      .navigate(uri)
      .build()

    webview.run()
  }
}