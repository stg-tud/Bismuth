package com.daimpl.app

import com.daimpl.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import rdts.base.LocalUid

object SpreadsheetComponent {

  def createSampleSpreadsheet()(using LocalUid): SpreadsheetDeltaAggregator[Spreadsheet[String]] = {
    new SpreadsheetDeltaAggregator(Spreadsheet[String]())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.addColumn())
  }

  case class Props(
      spreadsheetAggregator: SpreadsheetDeltaAggregator[Spreadsheet[String]],
      onDelta: Spreadsheet[String] => Callback,
      replicaId: LocalUid
  )

  case class State(
      editingCell: Option[(Int, Int)],
      editingValue: String,
      selectedRow: Option[Int],
      selectedColumn: Option[Int],
      conflictPopup: Option[(Int, Int)]
  )

  class Backend($ : BackendScope[Props, State]) {

    val replicaEventPrint: (LocalUid, String) => Callback = (replicaId, msg) => Callback(println(s"[${replicaId.show}]: ${msg}"))

    private def modSpreadsheet(f: (LocalUid) ?=> (Spreadsheet[String] => Spreadsheet[String])): Callback = {
      $.props.flatMap { props =>
        given LocalUid = props.replicaId
        val delta      = props.spreadsheetAggregator.editAndGetDelta(f)
        props.spreadsheetAggregator.visit(_.printToConsole())
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

    def handleDoubleClick(rowIdx: Int, colIdx: Int): Callback =
      $.props.flatMap { props =>
        val currentSet = props.spreadsheetAggregator.current.read(colIdx, rowIdx)
        val firstValue = currentSet.getFirstOrEmpty.getOrElse("")
        $.modState(_.copy(editingCell = Some((rowIdx, colIdx)), editingValue = firstValue))
      }

    def openConflict(row: Int, col: Int): Callback =
      $.modState(_.copy(conflictPopup = Some((row, col))))

    def closeConflict(): Callback =
      $.modState(_.copy(conflictPopup = None))

    def keepValue(row: Int, col: Int, v: String): Callback =
      modSpreadsheet(_.editCell(row, col, v)) >> closeConflict()

    def handleInputChange(e: ReactEventFromInput): Callback = {
      val value = e.target.value
      println(s"Edit value: ${value}")
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
            var value = state.editingValue.trim
            if (value.isBlank) value = null
            modSpreadsheet(_.editCell(rowIdx, colIdx, value)) >> cancelEdit()
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
      withSelectedRowAndProps((rowIdx, props) =>
        replicaEventPrint(props.replicaId, s"Inserting Row Before ${rowIdx + 1}")
        >>
        println(s"Inserting Row Before ${rowIdx}")
        modSpreadsheet(_.insertRow(rowIdx))
        >> $.modState(st => st.copy(selectedRow = Option(st.selectedRow.get - 1)))
      )

    def insertRowBelow(): Callback =
      withSelectedRowAndProps { (rowIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if (rowIdx == spreadsheet.numRows - 1) then modSpreadsheet(_.addRow())
          else modSpreadsheet(_.insertRow(rowIdx + 1))
        replicaEventPrint(props.replicaId, s"Inserting Row After ${rowIdx + 1}")
        >> action
        >> $.modState(st => st.copy(selectedRow = Option(st.selectedRow.get + 1)))
      }

    def removeRow(): Callback =
      withSelectedRowAndProps((rowIdx, props) =>
        replicaEventPrint(props.replicaId, s"Removing Row ${rowIdx + 1}")
        >> modSpreadsheet(_.removeRow(rowIdx))
        //>> modSpreadsheet(_.purgeTombstones())
        >> $.modState(_.copy(selectedRow = None))
      )

    def insertColumnLeft(): Callback =
      withSelectedColumnAndProps((colIdx, props) =>
        replicaEventPrint(props.replicaId, s"Inserting Column Before ${colIdx + 1}")
        >>
        println(s"Inserting Column Before ${colIdx}")
        modSpreadsheet(_.insertColumn(colIdx))
        >> $.modState(st => st.copy(selectedColumn = Some(st.selectedColumn.get - 1)))
      )

    def insertColumnRight(): Callback =
      withSelectedColumnAndProps { (colIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if (colIdx == spreadsheet.numColumns - 1) then modSpreadsheet(_.addColumn())
          else modSpreadsheet(_.insertColumn(colIdx + 1))
        replicaEventPrint(props.replicaId, s"Inserting Column After ${colIdx + 1}")
        >> action
        >> $.modState(st => st.copy(selectedColumn = Some(st.selectedColumn.get + 1)))
      }

    def removeColumn(): Callback =
      withSelectedColumnAndProps((colIdx, props) =>
        replicaEventPrint(props.replicaId, s"Removing Column ${colIdx + 1}")
        >> modSpreadsheet(_.removeColumn(colIdx))
        //>> modSpreadsheet(_.purgeTombstones())
        >> $.modState(_.copy(selectedColumn = None))
      )

    def addRow(): Callback = modSpreadsheet(_.addRow())

    def addColumn(): Callback = modSpreadsheet(_.addColumn())

    def purgeTombstones(): Callback = modSpreadsheet(_.purgeTombstones())
  }

  val Component = ScalaComponent
    .builder[Props]("Spreadsheet")
    .initialState(State(None, "", None, None, None))
    .backend(new Backend(_))
    .render { $ =>
      val props       = $.props
      val state       = $.state
      val backend     = $.backend
      val spreadsheet = props.spreadsheetAggregator.current
      val data        = spreadsheet.toList

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
            case None =>
              if spreadsheet.numRows == 0 then
                <.div(
                  ^.className := "flex gap-2 items-center",
                  <.span(^.className := "text-sm text-gray-600", s"No rows present."),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.addRow(),
                    "Insert"
                  )
                )
              else <.span()
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
            case None =>
              if spreadsheet.numColumns == 0 then
                <.div(
                  ^.className := "flex gap-2 items-center",
                  <.span(^.className := "text-sm text-gray-600", s"No columns present."),
                  <.button(
                    ^.className := "px-2 py-1 bg-green-500 text-white rounded hover:bg-green-600 text-xs",
                    ^.onClick --> backend.addColumn(),
                    "Insert"
                  )
                )
              else <.span()
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
                      ^.key       := s"header-$i",
                      ^.className := {
                        val baseClass =
                          "border border-gray-300 px-4 py-2 font-semibold w-32 max-w-32 cursor-pointer hover:bg-gray-200"
                        if state.selectedColumn.contains(i) then baseClass + " bg-blue-200"
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
                      if state.selectedRow.contains(rowIdx) then baseClass + " bg-blue-200"
                      else baseClass + " bg-gray-50"
                    },
                    ^.onClick --> backend.selectRow(rowIdx),
                    ^.title := "Click to select row",
                    (rowIdx + 1).toString
                  ),
                  row.zipWithIndex.map { case (cell, colIdx) =>
                    <.td(
                      ^.key       := s"cell-$rowIdx-$colIdx",
                      ^.className := "border border-gray-300 px-4 py-2 text-center relative w-32 max-w-32",
                      ^.onDoubleClick --> backend.handleDoubleClick(rowIdx, colIdx),
                      if cell.hasConflicts then
                        <.span(
                          ^.className := "absolute top-0 right-0 mr-1 mt-1 text-m cursor-pointer text-red-600",
                          ^.title := "Resolve conflict",
                          "!",
                          ^.onClick --> backend.openConflict(rowIdx, colIdx)
                        )
                      else EmptyVdom,
                      state.editingCell match {
                        case Some((editRow, editCol)) if editRow == rowIdx && editCol == colIdx =>
                          <.input(
                            ^.`type` := "text",
                            ^.value  := state.editingValue,
                            ^.onChange ==> backend.handleInputChange,
                            ^.onKeyPress ==> backend.handleKeyPress,
                            ^.className := "w-full h-full border-none outline-none px-2 py-1 max-w-full",
                            ^.autoFocus := true
                          )
                        case _ =>
                          <.span(
                            ^.className := "block cursor-pointer min-h-[1.5rem] overflow-hidden text-ellipsis whitespace-nowrap",
                            ^.title := cell.formatConflicts(),
                            cell.formatConflicts()
                          )
                      }
                    )
                  }.toVdomArray
                )
              }.toVdomArray
            )
          )
        ),
        <.div(
          ^.className := "mt-4 flex flex-wrap gap-2",
          <.button(
            ^.className := "px-2 py-1 bg-purple-500 text-white rounded hover:bg-purple-600 text-xs",
            ^.onClick --> backend.purgeTombstones(),
            "Purge Tombstones"
          )
        )
        (state.conflictPopup match
          case Some((r, c)) =>
            val vals = props.spreadsheetAggregator.current.read(c, r).toList
            <.div(
              ^.className := "fixed inset-0 z-50 flex items-center justify-center bg-black/50 backdrop-blur-sm transition-opacity duration-200",
              ^.onClick --> backend.closeConflict(),
              <.div(
                ^.className := "bg-white rounded-2xl shadow-2xl p-6 w-full max-w-sm animate-scale-in",
                ^.onClick ==> {
                  _.stopPropagationCB
                },
                <.button(
                  ^.className := "absolute top-4 right-4 text-gray-400 hover:text-gray-600",
                  ^.aria.label := "Close",
                  ^.onClick --> backend.closeConflict(),
                  "✕"
                ),
                <.h3(^.className := "text-lg font-semibold text-gray-800 mb-4", "Resolve conflict"),
                <.p(
                  ^.className := "text-sm text-gray-600 mb-3",
                  s"Choose the value you want to keep for cell ${(c + 'A').toChar}${r + 1}:"
                ),
                <.div(
                  ^.className := "flex flex-col gap-2",
                  vals.map { v =>
                    <.button(
                      ^.key := v,
                      ^.className := "w-full px-4 py-2 rounded-lg text-left bg-purple-50 hover:bg-purple-100 focus:outline-none focus:ring-2 focus:ring-purple-400",
                      ^.onClick --> backend.keepValue(r, c, v),
                      v
                    )
                  }.toVdomArray
                )
              )
            )
          case None => EmptyVdom
        )
      )
    }
    .build
}
