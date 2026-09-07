package ex2026webview

import io.avaje.webview.Webview as AvajeWebview

import java.nio.file.Path

/** Desktop (JVM/FFM) webview launcher backed directly by avaje-webview.
  *
  * Renders the built exWeb examples (`Modules/exWeb/target/dist/index.html`)
  * in a native window using the avaje-webview API directly.
  */
object Webview {
  def main(args: Array[String]): Unit = {
    if args.isEmpty then
        println(s"requires a path to the html as a first argument")
        return

    val webview = AvajeWebview
      .builder()
      .title("Bismuth Web Apps")
      .enableDeveloperTools(true) // right-click > Inspect
      .navigate(Path.of(args.head).toUri.toString)
      .build()

    webview.run()
  }
}
