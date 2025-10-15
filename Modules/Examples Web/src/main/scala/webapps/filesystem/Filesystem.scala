package webapps.filesystem

import org.scalajs.dom.{document, window}
import scala.scalajs.js.annotation.JSExportTopLevel
import rdts.base.LocalUid

object Filesystem {

  given replicaId: LocalUid = LocalUid.gen()

  @JSExportTopLevel("Filesystem")
  def run(): Unit = {
    val storagePrefix = window.location.href
    println(storagePrefix)

    val calendar = new FilesystemUI(storagePrefix, replicaId)
    val div      = calendar.getContents()

    document.getElementById("app").replaceChildren(div)

    ()
  }

}
