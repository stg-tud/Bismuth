package webapps.ex2026minisocial

import org.scalajs.dom.document
import rdts.base.ReplicaId
import webapps.DemoBridge

import scala.scalajs.js.annotation.JSExportTopLevel

object MiniSocialMain {

  val replicaId: ReplicaId = ReplicaId.gen()

  @JSExportTopLevel("MiniSocial")
  def run(): Unit = {
    val content = MiniSocialUI.getContents()

    val webrtc = DemoBridge.connectionView(MiniSocialDataManager.dataManager)

    val container = document.getElementById("app")
    container.replaceChildren(content)
    container.appendChild(webrtc)
    ()
  }

}
