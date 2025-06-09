package com.daimpl.app

import com.daimpl.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom
import rdts.base.Lattice
import rdts.base.LocalUid
import rdts.dotted.Obrem

object Main {
  case class SpreadsheetData(
      id: Int,
      isOnline: Boolean,
      aggregator: SpreadsheetDeltaAggregator[Spreadsheet],
      replicaId: LocalUid
  )
  case class State(spreadsheets: List[SpreadsheetData], nextId: Int)

  class Backend($ : BackendScope[Unit, State]) {
    def addSpreadsheet(): Callback = {
      $.modState { state =>
        val onlineSpreadsheets = state.spreadsheets.filter(_.isOnline)
        given LocalUid = LocalUid.gen()

        val newAggregator =
          if (onlineSpreadsheets.isEmpty) {
            SpreadsheetComponent.createSampleSpreadsheet()
          } else {
            val mergedObrem = onlineSpreadsheets
              .map(_.aggregator.getObrem)
              .reduce((s1, s2) => Lattice[Obrem[Spreadsheet]].merge(s1, s2))
            new SpreadsheetDeltaAggregator(mergedObrem)
          }

        state.copy(
          spreadsheets = state.spreadsheets :+ SpreadsheetData(state.nextId, isOnline = true, newAggregator, summon),
          nextId = state.nextId + 1
        )
      }
    }

    def removeSpreadsheet(id: Int): Callback = {
      $.modState(state => state.copy(spreadsheets = state.spreadsheets.filter(_.id != id)))
    }

    def toggleOnlineStatus(id: Int): Callback = {
      $.modState { state =>
        state.spreadsheets.find(_.id == id) match {
          case Some(sheet) if sheet.isOnline => // Is online -> turn offline
            val updatedSpreadsheets = state.spreadsheets.map(s => if (s.id == id) s.copy(isOnline = false) else s)
            state.copy(spreadsheets = updatedSpreadsheets)

          case Some(sheet) => // Is offline -> turn online and sync
            val otherOnlineSheets = state.spreadsheets.filter(s => s.id != id && s.isOnline)
            val sheetToSyncObrem = sheet.aggregator.getObrem

            val updatedSpreadsheets = state.spreadsheets.map {
              case s if s.id == id => // The sheet that is coming online
                val mergedAggregator = otherOnlineSheets.foldLeft(s.aggregator) { (agg, other) =>
                  agg.merge(other.aggregator.getObrem)
                }
                s.copy(isOnline = true, aggregator = mergedAggregator)

              case s if s.isOnline => // An already online sheet
                s.copy(aggregator = s.aggregator.merge(sheetToSyncObrem))

              case s => s // An offline sheet
            }
            state.copy(spreadsheets = updatedSpreadsheets)

          case None => state // sheet not found
        }
      }
    }

    def handleDelta(sourceSheetId: Int, delta: Obrem[Spreadsheet]): Callback = {
      $.modState { state =>
        val updatedSpreadsheets = state.spreadsheets.map { sheet =>
          if (sheet.id != sourceSheetId && sheet.isOnline) {
            sheet.copy(aggregator = sheet.aggregator.merge(delta))
          } else {
            sheet
          }
        }
        state.copy(spreadsheets = updatedSpreadsheets)
      }
    }
  }

  val App = ScalaComponent
    .builder[Unit]("App")
    .initialState {
      given LocalUid = LocalUid.gen()
      State(
        List(SpreadsheetData(1, isOnline = true, SpreadsheetComponent.createSampleSpreadsheet(), summon)),
        2
      )
    }
    .backend(new Backend(_))
    .render { $ =>
      val state = $.state
      val backend = $.backend

      <.div(
        ^.className := "min-h-screen bg-gradient-to-br from-blue-400 to-purple-600 p-8",
        <.div(
          ^.className := "flex justify-center gap-4 mb-8",
          <.button(
            ^.className := "px-6 py-3 rounded-lg font-semibold text-white bg-green-500 hover:bg-green-600 shadow-lg hover:shadow-xl transition-all duration-200",
            ^.onClick --> backend.addSpreadsheet(),
            "Add Spreadsheet"
          )
        ),
        <.div(
          ^.className := "grid grid-cols-1 lg:grid-cols-2 gap-8 max-w-full",
          state.spreadsheets.map { sheetData =>
            <.div(
              ^.key := s"spreadsheet-${sheetData.id}",
              ^.className := "bg-white rounded-lg shadow-2xl p-8 max-w-4xl mx-auto",
              SpreadsheetControls.Component(
                SpreadsheetControls.Props(
                  spreadsheetId = sheetData.id,
                  isOnline = sheetData.isOnline,
                  onToggleOnline = backend.toggleOnlineStatus,
                  onRemove = backend.removeSpreadsheet
                )
              ),
              SpreadsheetComponent.Component(
                SpreadsheetComponent.Props(
                  spreadsheetAggregator = sheetData.aggregator,
                  onDelta = if (sheetData.isOnline) delta => backend.handleDelta(sheetData.id, delta)
                  else _ => Callback.empty,
                  replicaId = sheetData.replicaId
                )
              )
            )
          }.toVdomArray
        )
      )
    }
    .build

  def main(args: Array[String]): Unit = {
    val container = dom.document.getElementById("root")
    App().renderIntoDOM(container)
  }
}
