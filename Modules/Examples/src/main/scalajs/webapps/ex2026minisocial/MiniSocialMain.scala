package webapps.ex2026minisocial

import org.scalajs.dom.document
import rdts.base.{Lattice, ReplicaId}
import reactives.extra.Tags.reattach
import scalatags.JsDom.all
import webapps.DemoBridge

import scala.scalajs.js.annotation.JSExportTopLevel

object MiniSocialMain {

  val replicaId: ReplicaId = ReplicaId.gen()

  @JSExportTopLevel("MiniSocial")
  def run(): Unit = {
    val content = MiniSocialUI.getContents()

    val statusInfo = all.div.render.reattach(
      MiniSocialDataManager.receivedCallback.map { _ =>
        val state = MiniSocialDataManager.replication.appPayloads.reduceOption(Lattice.merge)
        all.div(
          all.pre(all.stringFrag(pprint.apply(state).plainText)),
          all.br(),
          all.pre(all.stringFrag(pprint.apply(MiniSocialDataManager.dataManager.selfPeer).plainText))
        ).render
      }.hold(all.span.render)
    )

    val webrtc = DemoBridge.connectionView(MiniSocialDataManager.dataManager)

    val container = document.getElementById("app")
    container.replaceChildren(content)
    container.appendChild(webrtc)
    container.appendChild(statusInfo)
    ()
  }

}
