package todo

import dtn.rdt.Channel
import org.scalajs.dom.experimental.webrtc
import org.scalajs.dom.{document, window}
import rdts.base.{Lattice, LocalUid}
import reactives.extra.Tags.reattach
import replication.WebRTCConnectionView
import scalatags.JsDom.all
import scalatags.JsDom.all.given
import todo.TodoDataManager.TodoRepState

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

  lazy val webrtc = WebRTCConnectionView(TodoDataManager.dataManager).example()

  val updateTask: TimerTask = { () =>
    TodoDataManager.dataManager.requestData()
  }

  @JSExportTopLevel("Todolist")
  def run(): Unit = {

    val container = document.getElementById("app")

    container.replaceChildren(contents)

    container.appendChild(DTNTestConnector.getConnectorContents())

    container.appendChild(webrtc.render)

    container.appendChild {
      all.div.render.reattach(TodoDataManager.receivedCallback.map(_ =>
        val state = TodoDataManager.dataManager.deltaStorage.allPayloads.map(_.payload.data).reduceOption(Lattice.merge)
        all.pre(all.stringFrag(pprint.apply(state).plainText)).render
      ).hold(all.span.render))
    }

    updateTask.cancel()

    timer.scheduleAtFixedRate(
      updateTask,
      1000,
      1000
    )
    ()
  }

}

object DTNTestConnector {
  def getConnectorContents() = {
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
