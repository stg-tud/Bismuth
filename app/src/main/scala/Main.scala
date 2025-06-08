import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom
import com.daimpl.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import rdts.base.LocalUid
import rdts.time.Dots
import rdts.dotted.{Dotted, Obrem}

object Main {

  def createSampleSpreadsheet(): SpreadsheetDeltaAggregator[Spreadsheet] = {
    given LocalUid = LocalUid.gen()
    val spreadsheet = SpreadsheetDeltaAggregator(Obrem(Spreadsheet()))

    spreadsheet
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.editCell(0, 0, "Hello World!"))
      .edit(_.editCell(1, 0, "Still here!"))
      .edit(_.addRow())
      .edit(_.addColumn())
      .edit(_.editCell(0, 0, "1"))
      .edit(_.editCell(0, 1, "2"))
      .edit(_.editCell(1, 0, "3"))
      .edit(_.editCell(1, 1, "4"))
      .edit(_.insertRow(1))
      .edit(_.insertColumn(0))

    spreadsheet
  }

  case class State(
    spreadsheet: SpreadsheetDeltaAggregator[Spreadsheet],
    editingCell: Option[(Int, Int)],
    editingValue: String,
    selectedRow: Option[Int],
    selectedColumn: Option[Int]
  )

  class Backend($: BackendScope[Unit, State]) {
    given LocalUid = LocalUid.gen()

    def handleDoubleClick(rowIdx: Int, colIdx: Int): Callback = {
      $.modState { state =>
        val currentValue = state.spreadsheet.current.read(colIdx, rowIdx).getOrElse("")
        state.copy(
          editingCell = Some((rowIdx, colIdx)),
          editingValue = currentValue
        )
      }
    }

    def handleInputChange(e: ReactEventFromInput): Callback = {
      val value = e.target.value
      $.modState(_.copy(editingValue = value))
    }

    def handleKeyPress(e: ReactKeyboardEvent): Callback = {
      if (e.key == "Enter") {
        commitEdit()
      } else if (e.key == "Escape") {
        cancelEdit()
      } else {
        Callback.empty
      }
    }

    def commitEdit(): Callback = {
      $.modState { state =>
        state.editingCell match {
          case Some((rowIdx, colIdx)) =>
            val newSpreadsheet = state.spreadsheet
              .edit(_.editCell(rowIdx, colIdx, if (state.editingValue.trim.isEmpty) null else state.editingValue))
            
            state.copy(
              spreadsheet = newSpreadsheet,
              editingCell = None,
              editingValue = ""
            )
          case None => state
        }
      }
    }

    def cancelEdit(): Callback = {
      $.modState(_.copy(editingCell = None, editingValue = ""))
    }

    def selectRow(rowIdx: Int): Callback = {
      $.modState(_.copy(selectedRow = Some(rowIdx), selectedColumn = None))
    }

    def selectColumn(colIdx: Int): Callback = {
      $.modState(_.copy(selectedColumn = Some(colIdx), selectedRow = None))
    }

    def insertRowAbove(): Callback = {
      $.modState { state =>
        state.selectedRow match {
          case Some(rowIdx) =>
            val newSpreadsheet = state.spreadsheet.edit(_.insertRow(rowIdx))
            state.copy(spreadsheet = newSpreadsheet, selectedRow = None)
          case None => state
        }
      }
    }

    def insertRowBelow(): Callback = {
      $.modState { state =>
        state.selectedRow match {
          case Some(rowIdx) =>
            val newSpreadsheet = state.spreadsheet.edit(_.insertRow(rowIdx + 1))
            state.copy(spreadsheet = newSpreadsheet, selectedRow = None)
          case None => state
        }
      }
    }

    def removeRow(): Callback = {
      $.modState { state =>
        state.selectedRow match {
          case Some(rowIdx) if state.spreadsheet.current.numRows > 1 =>
            val newSpreadsheet = state.spreadsheet
              .edit(_.removeRow(rowIdx))
              .edit(_.purgeTombstones())
            state.copy(spreadsheet = newSpreadsheet, selectedRow = None)
          case _ => state
        }
      }
    }

    def insertColumnLeft(): Callback = {
      $.modState { state =>
        state.selectedColumn match {
          case Some(colIdx) =>
            val newSpreadsheet = state.spreadsheet.edit(_.insertColumn(colIdx))
            state.copy(spreadsheet = newSpreadsheet, selectedColumn = None)
          case None => state
        }
      }
    }

    def insertColumnRight(): Callback = {
      $.modState { state =>
        state.selectedColumn match {
          case Some(colIdx) =>
            val newSpreadsheet = state.spreadsheet.edit(_.insertColumn(colIdx + 1))
            state.copy(spreadsheet = newSpreadsheet, selectedColumn = None)
          case None => state
        }
      }
    }

    def removeColumn(): Callback = {
      $.modState { state =>
        dom.console.log(state.spreadsheet.current.numColumns)
        dom.console.log(state.selectedColumn)
        state.selectedColumn match {
          case Some(colIdx) if state.spreadsheet.current.numColumns > 1 =>
            val newSpreadsheet = state.spreadsheet.edit(_.removeColumn(colIdx)).edit(_.purgeTombstones())
            newSpreadsheet.current.printToConsole()
            state.copy(spreadsheet = newSpreadsheet, selectedColumn = None)
          case _ => state
        }
      }
    }

    def addRow(): Callback = {
      $.modState { state =>
        val newSpreadsheet = state.spreadsheet.edit(_.addRow())
        state.copy(spreadsheet = newSpreadsheet)
      }
    }

    def addColumn(): Callback = {
      $.modState { state =>
        val newSpreadsheet = state.spreadsheet.edit(_.addColumn())
        state.copy(spreadsheet = newSpreadsheet)
      }
    }
  }

  val SpreadsheetDisplay = ScalaComponent.builder[Unit]("SpreadsheetDisplay")
    .initialState(State(createSampleSpreadsheet(), None, "", None, None))
    .backend(new Backend(_))
    .render { $ =>
      val state = $.state
      val backend = $.backend
      val spreadsheet = state.spreadsheet.current
      val data = spreadsheet.toList

      <.div(
        ^.className := "min-h-screen bg-gradient-to-br from-blue-400 to-purple-600 flex items-center justify-center",
        <.div(
          ^.className := "bg-white rounded-lg shadow-2xl p-8 max-w-4xl mx-auto",
          <.div(
            ^.className := "mb-4 flex flex-wrap gap-2",
            state.selectedRow match {
              case Some(rowIdx) =>
                <.div(
                  ^.className := "flex gap-2 items-center",
                  <.span(^.className := "text-sm text-gray-600", s"Row ${rowIdx + 1} selected:"),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.insertRowAbove(),
                    "Insert Above"
                  ),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.insertRowBelow(),
                    "Insert Below"
                  ),
                  <.button(
                    ^.className := "px-2 py-1 bg-red-500 text-white rounded hover:bg-red-600 text-xs",
                    ^.onClick --> backend.removeRow(),
                    "Remove Row"
                  )
                )
              case None => <.span()
            },
            state.selectedColumn match {
              case Some(colIdx) =>
                <.div(
                  ^.className := "flex gap-2 items-center",
                  <.span(^.className := "text-sm text-gray-600", s"Column ${(colIdx + 'A').toChar} selected:"),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.insertColumnLeft(),
                    "Insert Left"
                  ),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.insertColumnRight(),
                    "Insert Right"
                  ),
                  <.button(
                    ^.className := "px-2 py-1 bg-red-500 text-white rounded hover:bg-red-600 text-xs",
                    ^.onClick --> backend.removeColumn(),
                    "Remove Column"
                  )
                )
              case None => <.span()
            }
          ),
          <.div(
            ^.className := "overflow-x-auto",
            <.table(
              ^.className := "min-w-full border-collapse border border-gray-300",
              <.thead(
                <.tr(
                  <.th(
                    ^.className := "border border-gray-300 px-4 py-2 bg-gray-50 font-semibold w-16",
                    "#"
                  ),
                  (0 until spreadsheet.numColumns).map(i =>
                    <.th(
                      ^.key := s"header-$i",
                      ^.className := {
                        val baseClass = "border border-gray-300 px-4 py-2 font-semibold w-32 max-w-32 cursor-pointer hover:bg-gray-200"
                        if (state.selectedColumn.contains(i)) baseClass + " bg-blue-200"
                        else baseClass + " bg-gray-100"
                      },
                      ^.onClick --> backend.selectColumn(i),
                      ^.title := "Click to select column",
                      (i + 'A').toChar.toString
                    )
                  ).toVdomArray
                )
              ),
              <.tbody(
                data.zipWithIndex.map { case (row, rowIdx) =>
                  <.tr(
                    ^.key := s"row-$rowIdx",
                    <.td(
                      ^.className := {
                        val baseClass = "border border-gray-300 px-4 py-2 font-medium text-center cursor-pointer hover:bg-gray-200"
                        if (state.selectedRow.contains(rowIdx)) baseClass + " bg-blue-200"
                        else baseClass + " bg-gray-50"
                      },
                      ^.onClick --> backend.selectRow(rowIdx),
                      ^.title := "Click to select row",
                      (rowIdx + 1).toString
                    ),
                    row.zipWithIndex.map { case (cell, colIdx) =>
                      <.td(
                        ^.key := s"cell-$rowIdx-$colIdx",
                        ^.className := "border border-gray-300 px-4 py-2 text-center relative w-32 max-w-32",
                        ^.onDoubleClick --> backend.handleDoubleClick(rowIdx, colIdx),
                        state.editingCell match {
                          case Some((editRow, editCol)) if editRow == rowIdx && editCol == colIdx =>
                            <.input(
                              ^.`type` := "text",
                              ^.value := state.editingValue,
                              ^.onChange ==> backend.handleInputChange,
                              ^.onKeyPress ==> backend.handleKeyPress,
                              ^.className := "w-full h-full border-none outline-none px-2 py-1 max-w-full",
                              ^.autoFocus := true
                            )
                          case _ =>
                            <.span(
                              ^.className := "block cursor-pointer min-h-[1.5rem] overflow-hidden text-ellipsis whitespace-nowrap",
                              ^.title := s"Double-click to edit: ${cell.getOrElse("")}",
                              cell.getOrElse("")
                            )
                        }
                      )
                    }.toVdomArray
                  )
                }.toVdomArray
              )
            )
          )
        )
      )
    }
    .build

  def main(args: Array[String]): Unit = {
    val container = dom.document.getElementById("root")
    SpreadsheetDisplay().renderIntoDOM(container)
  }
} 