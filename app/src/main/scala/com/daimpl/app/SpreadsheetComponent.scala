package com.daimpl.app

import com.daimpl.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import rdts.base.{Lattice, LocalUid}
import rdts.dotted.Obrem
import rdts.time.Dots

object SpreadsheetComponent {

  def createSampleSpreadsheet()(using LocalUid): SpreadsheetDeltaAggregator[Spreadsheet] = {
    new SpreadsheetDeltaAggregator(Obrem(Spreadsheet()))
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.addColumn())
  }

  case class Props(
      spreadsheetAggregator: SpreadsheetDeltaAggregator[Spreadsheet],
      onDelta: Obrem[Spreadsheet] => Callback,
      replicaId: LocalUid
  )

  case class State(
      editingCell: Option[(Int, Int)],
      editingValue: String,
      selectedRow: Option[Int],
      selectedColumn: Option[Int]
  )

  class Backend($ : BackendScope[Props, State]) {

    private def modSpreadsheet(f: (LocalUid) ?=> (Dots ?=> Spreadsheet => Obrem[Spreadsheet])): Callback = {
      $.props.flatMap { props =>
        given LocalUid = props.replicaId
        val delta = props.spreadsheetAggregator.editAndGetDelta(f)
        props.onDelta(delta)
      }
    }

    private def withSelectedRow(f: Int => Callback): Callback =
      $.state.flatMap(_.selectedRow.map(f).getOrElse(Callback.empty))

    private def withSelectedColumn(f: Int => Callback): Callback =
      $.state.flatMap(_.selectedColumn.map(f).getOrElse(Callback.empty))

    private def withSelectedRowAndProps(f: (Int, Props) => Callback): Callback =
      withSelectedRow(rowIdx => $.props.flatMap(props => f(rowIdx, props)))

    private def withSelectedColumnAndProps(f: (Int, Props) => Callback): Callback =
      withSelectedColumn(colIdx => $.props.flatMap(props => f(colIdx, props)))

    def handleDoubleClick(rowIdx: Int, colIdx: Int): Callback = {
      $.props.flatMap { props =>
        val currentValue = props.spreadsheetAggregator.current.read(colIdx, rowIdx).getOrElse("")
        $.modState(_.copy(editingCell = Some((rowIdx, colIdx)), editingValue = currentValue))
      }
    }

    def handleInputChange(e: ReactEventFromInput): Callback = {
      val value = e.target.value
      $.modState(_.copy(editingValue = value))
    }

    def handleKeyPress(e: ReactKeyboardEvent): Callback = {
      e.key match {
        case "Enter"  => commitEdit()
        case "Escape" => cancelEdit()
        case _        => Callback.empty
      }
    }

    def commitEdit(): Callback = {
      $.state.flatMap { state =>
        state.editingCell
          .map { case (rowIdx, colIdx) =>
            val value = state.editingValue.trim
            modSpreadsheet(_.editCell(rowIdx, colIdx, if (value.isEmpty) null else value)) >> cancelEdit()
          }
          .getOrElse(Callback.empty)
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

    def insertRowAbove(): Callback =
      withSelectedRow(rowIdx => modSpreadsheet(_.insertRow(rowIdx)) >> $.modState(_.copy(selectedRow = None)))

    def insertRowBelow(): Callback =
      withSelectedRowAndProps { (rowIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if (rowIdx == spreadsheet.numRows - 1) modSpreadsheet(_.addRow())
          else modSpreadsheet(_.insertRow(rowIdx + 1))
        action >> $.modState(_.copy(selectedRow = None))
      }

    def removeRow(): Callback =
      withSelectedRow(rowIdx =>
        modSpreadsheet(_.removeRow(rowIdx)) >> modSpreadsheet(_.purgeTombstones()) >> $.modState(
          _.copy(selectedRow = None)
        )
      )

    def insertColumnLeft(): Callback =
      withSelectedColumn(colIdx => modSpreadsheet(_.insertColumn(colIdx)) >> $.modState(_.copy(selectedColumn = None)))

    def insertColumnRight(): Callback =
      withSelectedColumnAndProps { (colIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if (colIdx == spreadsheet.numColumns - 1) modSpreadsheet(_.addColumn())
          else modSpreadsheet(_.insertColumn(colIdx + 1))
        action >> $.modState(_.copy(selectedColumn = None))
      }

    def removeColumn(): Callback =
      withSelectedColumn(colIdx =>
        modSpreadsheet(_.removeColumn(colIdx)) >> modSpreadsheet(_.purgeTombstones()) >> $.modState(
          _.copy(selectedColumn = None)
        )
      )

    def addRow(): Callback = modSpreadsheet(_.addRow())

    def addColumn(): Callback = modSpreadsheet(_.addColumn())
  }

  val Component = ScalaComponent
    .builder[Props]("Spreadsheet")
    .initialState(State(None, "", None, None))
    .backend(new Backend(_))
    .render { $ =>
      val props = $.props
      val state = $.state
      val backend = $.backend
      val spreadsheet = props.spreadsheetAggregator.current
      val data = spreadsheet.toList

      <.div(
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
                (0 until spreadsheet.numColumns)
                  .map(i =>
                    <.th(
                      ^.key := s"header-$i",
                      ^.className := {
                        val baseClass =
                          "border border-gray-300 px-4 py-2 font-semibold w-32 max-w-32 cursor-pointer hover:bg-gray-200"
                        if (state.selectedColumn.contains(i)) baseClass + " bg-blue-200"
                        else baseClass + " bg-gray-100"
                      },
                      ^.onClick --> backend.selectColumn(i),
                      ^.title := "Click to select column",
                      (i + 'A').toChar.toString
                    )
                  )
                  .toVdomArray
              )
            ),
            <.tbody(
              data.zipWithIndex.map { case (row, rowIdx) =>
                <.tr(
                  ^.key := s"row-$rowIdx",
                  <.td(
                    ^.className := {
                      val baseClass =
                        "border border-gray-300 px-4 py-2 font-medium text-center cursor-pointer hover:bg-gray-200"
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
                            ^.title := cell.getOrElse(""),
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
    }
    .build
}
