package webapps.todo

import dtn.rdt.Channel
import org.scalajs.dom.html.Div
import org.scalajs.dom.{document, window}
import rdts.base.{Lattice, LocalUid}
import reactives.extra.Tags.reattach
import scalatags.JsDom.all
import scalatags.JsDom.all.given
import webapps.WebRTCConnectionView
import webapps.todo.TodoDataManager.TodoRepState

import java.util.{Timer, TimerTask}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: LocalUid = LocalUid.gen()

  val timer = new Timer()

  lazy val contents = {
    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp = new TodoAppUI(storagePrefix)
    todoApp.getContents()
  }

  // TodoDataManager.dataManager.addLatentConnection(WebviewAdapterChannel.listen())

  lazy val webrtc = WebRTCConnectionView(TodoDataManager.dataManager).example().render

  lazy val updateTask: Unit = {

    timer.scheduleAtFixedRate(
      () => TodoDataManager.dataManager.requestData(),
      1000,
      1000
    )

  }

  lazy val dtnConnectorContents: Div = DTNTestConnector.getConnectorContents()

  lazy val statusInfo: Div = {
    all.div.render.reattach(TodoDataManager.receivedCallback.map(_ =>
      val state = TodoDataManager.dataManager.allPayloads.map(_.payload.data).reduceOption(Lattice.merge)
      all.pre(all.stringFrag(pprint.apply(state).plainText)).render
    ).hold(all.span.render))
  }

  @JSExportTopLevel("Todolist")
  def run(): Unit = {

    val container = document.getElementById("app")

    container.replaceChildren(contents)

    container.appendChild(dtnConnectorContents)

    container.appendChild(webrtc)

    container.appendChild(statusInfo)

    updateTask

  }

}

object DTNTestConnector {
  def getConnectorContents(): Div = {
    val portInput     = all.input(all.placeholder := "dtnd ws port").render
    val connectButton = all.button(all.onclick := { () =>
      TodoDataManager.dataManager.addObjectConnection(
        Channel[TodoRepState]("127.0.0.1", portInput.value.toInt, "app1", scala.concurrent.ExecutionContext.global)
      )
    }).render
    connectButton.textContent = "Connect"

    all.div(portInput, connectButton).render
  }
}
