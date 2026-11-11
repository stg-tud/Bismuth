package webapps.ex2019todo

import org.scalajs.dom.html.Div
import org.scalajs.dom.{Element, document, window}
import rdts.base.ReplicaId
import webapps.DemoBridge

import java.util.Timer
import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: ReplicaId = ReplicaId.gen()

  val timer = new Timer()

  lazy val contents: Div = {
    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp = new TodoAppUI(storagePrefix)
    todoApp.getContents()
  }

  // TodoDataManager.dataManager.addLatentConnection(WebviewAdapterChannel.listen())

  lazy val webrtc: Element = DemoBridge.connectionView(TodoDataManager.dataManager)

//  lazy val dtnConnectorContents: Div = DTNTestConnector.getConnectorContents()

  @JSExportTopLevel("Todolist")
  def run(): Unit = {

    val container = document.getElementById("app")

    container.replaceChildren(contents)

//    container.appendChild(dtnConnectorContents)

    container.appendChild(webrtc)

    ()
  }

}

//object DTNTestConnector {
//  def getConnectorContents(): Div = {
//    val portInput     = all.input(all.placeholder := "dtnd ws port").render
//    val connectButton = all.button(all.onclick := { () =>
//      TodoDataManager.dataManager.addConnection(
//        Channel[TodoRepState]("127.0.0.1", portInput.value.toInt, "app1", scala.concurrent.ExecutionContext.global)
//      )
//    }).render
//    connectButton.textContent = "Connect"
//
//    all.div(portInput, connectButton).render
//  }
//}
