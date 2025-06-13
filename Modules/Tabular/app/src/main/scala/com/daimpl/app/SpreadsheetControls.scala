package com.daimpl.app

import scala.language.implicitConversions
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

object SpreadsheetControls {

  case class Props(
      spreadsheetId: Int,
      isOnline: Boolean,
      onToggleOnline: Int => Callback,
      onRemove: Int => Callback
  )

  val Component = ScalaComponent
    .builder[Props]("SpreadsheetControls")
    .render_P { props =>
      <.div(
        ^.className := "flex justify-between items-center mb-4 p-3 bg-gray-50 rounded-lg border",
        <.div(
          ^.className := "flex items-center gap-3",
          <.h3(
            ^.className := "text-lg font-semibold text-gray-800",
            s"Spreadsheet #${props.spreadsheetId}"
          ),
          <.div(
            ^.className := "flex items-center gap-2",
            <.span(
              ^.className := s"w-3 h-3 rounded-full ${if props.isOnline then "bg-green-500" else "bg-red-500"}"
            ),
            <.span(
              ^.className := s"text-sm font-medium ${if props.isOnline then "text-green-700" else "text-red-700"}",
              if props.isOnline then "Online" else "Offline"
            )
          )
        ),
        <.div(
          ^.className := "flex gap-2",
          <.button(
            ^.className := s"px-4 py-2 rounded-lg font-medium transition-all duration-200 shadow-lg hover:shadow-xl ${
                if props.isOnline then
                  "bg-red-500 hover:bg-red-600 text-white"
                else
                  "bg-green-500 hover:bg-green-600 text-white"
              }",
            ^.onClick --> props.onToggleOnline(props.spreadsheetId),
            if props.isOnline then "Go Offline" else "Go Online"
          ),
          <.button(
            ^.className := "px-4 py-2 bg-red-500 hover:bg-red-600 text-white rounded-lg font-medium shadow-lg hover:shadow-xl transition-all duration-200",
            ^.onClick --> props.onRemove(props.spreadsheetId),
            "Delete"
          )
        )
      )
    }
    .build
}
