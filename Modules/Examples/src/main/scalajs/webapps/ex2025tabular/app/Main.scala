package webapps.ex2025tabular.app

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom.document
import rdts.base.ReplicaId
import webapps.ex2025tabular.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import webapps.{DemoBridge, OverlayReplication}

import scala.language.implicitConversions
import scala.scalajs.js.annotation.JSExportTopLevel

/** A single spreadsheet replica. Multiple replicas (and going offline) are handled by the demo page,
  * see [[webapps.DemoBridge]].
  */
object Main {

  val replicaId: ReplicaId = ReplicaId.gen()

  /** re-renders the app, set once mounted */
  private var refresh: () => Unit = () => ()

  val aggregator = SpreadsheetDeltaAggregator(SpreadsheetComponent.sampleSpreadsheet, replicaId)

  val replication: OverlayReplication[Spreadsheet[String]] =
    OverlayReplication[Spreadsheet[String]](
      replicaId,
      delta => {
        aggregator.accumulate(delta)
        refresh()
      }
    )

  @JSExportTopLevel("Tabular")
  def run(): Unit = {
    val container = document.getElementById("app")
    val reactRoot = document.createElement("div")
    container.replaceChildren(reactRoot, DemoBridge.connectionView(replication.io))
    ReactDOMClient.createRoot(reactRoot).render(App())
    ()
  }

  private val App = ScalaComponent
    .builder[Unit]("App")
    .initialState(0)
    .render { $ =>
      <.div(
        ^.className := "min-h-screen bg-gradient-to-br from-blue-400 to-purple-600 p-2 sm:p-8",
        <.div(
          ^.className := "bg-white rounded-lg shadow-2xl p-2 sm:p-8 max-w-4xl mx-auto overflow-x-auto",
          SpreadsheetComponent.Component(
            SpreadsheetComponent.Props(
              spreadsheetAggregator = aggregator,
              onDelta = delta => Callback(replication.broadcast(delta)),
              replicaId = replicaId
            )
          )
        )
      )
    }
    .componentDidMount { $ =>
      Callback { refresh = () => $.modState(_ + 1).runNow() }
    }
    .build
}
