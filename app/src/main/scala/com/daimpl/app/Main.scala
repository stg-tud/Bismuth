package com.daimpl.app

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom

object Main {
  case class SpreadsheetInfo(id: Int, isOnline: Boolean)
  case class State(spreadsheets: List[SpreadsheetInfo], nextId: Int)

  class Backend($ : BackendScope[Unit, State]) {
    def addSpreadsheet(): Callback = {
      $.modState(state =>
        state.copy(
          spreadsheets = state.spreadsheets :+ SpreadsheetInfo(state.nextId, isOnline = true),
          nextId = state.nextId + 1
        )
      )
    }

    def removeSpreadsheet(id: Int): Callback = {
      $.modState(state => state.copy(spreadsheets = state.spreadsheets.filter(_.id != id)))
    }

    def toggleOnlineStatus(id: Int): Callback = {
      $.modState(state =>
        state.copy(
          spreadsheets = state.spreadsheets.map(sheet =>
            if (sheet.id == id) sheet.copy(isOnline = !sheet.isOnline)
            else sheet
          )
        )
      )
    }
  }

  val App = ScalaComponent
    .builder[Unit]("App")
    .initialState(State(List(SpreadsheetInfo(1, isOnline = true)), 2))
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
          state.spreadsheets.map { sheetInfo =>
            <.div(
              ^.key := s"spreadsheet-${sheetInfo.id}",
              ^.className := "bg-white rounded-lg shadow-2xl p-8 max-w-4xl mx-auto",
              SpreadsheetControls.Component(
                SpreadsheetControls.Props(
                  spreadsheetId = sheetInfo.id,
                  isOnline = sheetInfo.isOnline,
                  onToggleOnline = backend.toggleOnlineStatus,
                  onRemove = backend.removeSpreadsheet
                )
              ),
              SpreadsheetComponent.Component()
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
