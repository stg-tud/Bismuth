package webapps.ex2026minisocial

import org.scalajs.dom.document
import rdts.base.{Lattice, LocalUid}
import reactives.extra.Tags.reattach
import scalatags.JsDom.all
import webapps.WebRTCConnectionView

import scala.scalajs.js.annotation.JSExportTopLevel

object MiniSocialMain {

  val replicaId: LocalUid = LocalUid.gen()

  @JSExportTopLevel("MiniSocial")
  def run(): Unit = {
    val content = MiniSocialUI.getContents()

    val statusInfo = all.div.render.reattach(
      MiniSocialDataManager.receivedCallback.map { _ =>
        val state = MiniSocialDataManager.dataManager.allPayloads.map(_.data).reduceOption(Lattice.merge)
        all.div(
          all.pre(all.stringFrag(pprint.apply(state).plainText)),
          all.br(),
          all.pre(all.stringFrag(pprint.apply(MiniSocialDataManager.dataManager.replicaId).plainText))
        ).render
      }.hold(all.span.render)
    )

    val webrtc = WebRTCConnectionView(MiniSocialDataManager.dataManager).example()

    val container = document.getElementById("app")
    container.replaceChildren(content)
    container.appendChild(webrtc.render)
    container.appendChild(statusInfo)
    ()
  }

}