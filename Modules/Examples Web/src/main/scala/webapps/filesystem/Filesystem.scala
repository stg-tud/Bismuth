package webapps.filesystem

import org.scalajs.dom.{document, window}
import rdts.base.Uid

import scala.scalajs.js.annotation.JSExportTopLevel

object Calendar {

  given replicaId: Uid = Uid.gen()

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
