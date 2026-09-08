package ex2026webview

import org.gnome.adw.{Application, ApplicationWindow, Bin, HeaderBar}
import org.gnome.gio.ApplicationFlags
import org.gnome.glib.{GLib, Uri}
import org.gnome.gobject.BindingFlags
import org.gnome.gtk.{Unit as _, *}
import org.webkitgtk.{LoadEvent, WebView}

import java.nio.file.Path

/** Desktop (JVM/GNOME) webview launcher backed by java-gi (GTK4 + Libadwaita + WebKitGTK)
  * instead of avaje-webview.
  *
  * Renders an exWeb page in a native window with a small browser chrome (back / forward /
  * stop-reload / home buttons and a URL bar). Accepts either a URL (e.g. a local vite dev
  * server: `http://localhost:5173/todolist.html`) or a path to a local html file.
  *
  * Requires the GTK4, Libadwaita and WebKitGTK native libraries at runtime.
  */
object JavaGiWebview {

  def main(args: Array[String]): Unit = {
    if args.isEmpty then
        println("requires a url or a path to an html file as a first argument")
        return

    val target = args.head
    val startUri =
      if target.startsWith("http://") || target.startsWith("https://") then target
      else Path.of(target).toUri.toString

    val app = new Application("org.bismuth.JavaGiWebview", ApplicationFlags.DEFAULT_FLAGS)
    app.onActivate(() => activate(app, startUri))

    app.run(Array[String]())
    ()
  }

  private def activate(app: Application, startUri: String): Unit = {
    // Webview component
    val webview = new WebView()

    // Disable smooth (animated) scrolling
    val settings = webview.getWebViewSettings
    settings.setEnableSmoothScrolling(false)
    webview.setSettings(settings)

    // Switches to a stop icon while loading, back to a reload icon when done.
    var loading = false

    // Back button
    val back = new Button()
    back.setIconName("go-previous-symbolic")
    back.setTooltipText("Back")
    back.onClicked(() => webview.goBack())

    // Forward button
    val forward = new Button()
    forward.setIconName("go-next-symbolic")
    forward.setTooltipText("Forward")
    forward.onClicked(() => webview.goForward())

    // Stop / Reload button
    val stopOrReload = new Button()
    stopOrReload.setIconName("process-stop-symbolic")
    stopOrReload.setTooltipText("Stop")
    stopOrReload.onClicked { () =>
      if loading then webview.stopLoading()
      else webview.reload()
    }

    // Home button
    val home = new Button()
    home.setIconName("go-home-symbolic")
    home.setTooltipText("Home")
    home.onClicked(() => webview.loadUri(startUri))

    // URL bar
    val urlBar = new Entry()
    urlBar.setInputPurpose(InputPurpose.URL)
    urlBar.setHexpand(true)

    // Container for the webview
    val viewContainer = new Bin()
    viewContainer.setVexpand(true)
    viewContainer.setHexpand(true)
    viewContainer.setChild(webview)

    // Keep the URL bar in sync with the webview's current uri
    webview.bindProperty("uri", urlBar.getBuffer, "text", BindingFlags.DEFAULT)

    // When the webview starts or finishes loading, switch the stop/reload icon and tooltip
    webview.onLoadChanged { event =>
      event match {
        case LoadEvent.STARTED =>
          loading = true
          stopOrReload.setIconName("process-stop-symbolic")
          stopOrReload.setTooltipText("Stop")
        case LoadEvent.FINISHED =>
          loading = false
          stopOrReload.setIconName("view-refresh-symbolic")
          stopOrReload.setTooltipText("Reload")
        case _ =>
          // Ignore all other events
      }
    }

    // When the user navigates to a new URL
    urlBar.onActivate { () =>
      var url = urlBar.getBuffer.getText
      if Uri.peekScheme(url) == null then url = "https://" + url
      webview.loadUri(url)
    }

    // Update the progress indicator in the URL bar during loading
    webview.onNotify(
      "estimated-load-progress",
      _ => {
        val progress = webview.getEstimatedLoadProgress
        urlBar.setProgressFraction(progress)
        if progress >= 1.0 then
            GLib.timeoutAddOnce(500, () => urlBar.setProgressFraction(0.0)): Unit
      },
    )

    // Start loading the initial page
    webview.loadUri(startUri)

    // Construct the header bar
    val headerbar = new HeaderBar()
    headerbar.packStart(back)
    headerbar.packStart(forward)
    headerbar.packStart(stopOrReload)
    headerbar.packStart(home)
    headerbar.setTitleWidget(urlBar)

    // Pack everything together, and show the window
    val box = new Box(Orientation.VERTICAL, 0)
    box.append(headerbar)
    box.append(viewContainer)

    val window = new ApplicationWindow(app)
    window.setDefaultSize(700, 700)
    window.setContent(box)
    window.present()
    ()
  }
}
