package com.daimpl.app

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom

object Main {

  val App = ScalaComponent.builder[Unit]("App")
    .render { _ =>
      <.div(
        ^.className := "min-h-screen bg-gradient-to-br from-blue-400 to-purple-600 flex items-center justify-center",
        SpreadsheetComponent.Component()
      )
    }
    .build

  def main(args: Array[String]): Unit = {
    val container = dom.document.getElementById("root")
    App().renderIntoDOM(container)
  }
} 