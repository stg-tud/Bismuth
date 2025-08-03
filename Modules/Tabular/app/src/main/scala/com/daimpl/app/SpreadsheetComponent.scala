package com.daimpl.app

import com.daimpl.lib.{Spreadsheet, SpreadsheetDeltaAggregator}
import com.daimpl.lib.Spreadsheet.SpreadsheetCoordinate
import japgolly.scalajs.react.*
import japgolly.scalajs.react.CtorType.Summoner.Aux
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.internal.Box
import japgolly.scalajs.react.{ReactDragEvent, ReactMouseEvent}
import japgolly.scalajs.react.vdom.html_<^.*

import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetTimeoutHandle
import org.scalajs.dom
import rdts.base.{LocalUid, Uid}

object SpreadsheetComponent {

  def createSampleSpreadsheet()(using LocalUid): SpreadsheetDeltaAggregator[String] = {
    new SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.gen())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addRow())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.addColumn())
      .edit(_.addColumn())
  }

  case class Props(
      spreadsheetAggregator: SpreadsheetDeltaAggregator[String],
      onDelta: Spreadsheet[String] => Callback,
      replicaId: LocalUid
  )

  case class State(
      editingCell: Option[(Int, Int)],
      editingValue: String,
      selectedRow: Option[Int],
      selectedColumn: Option[Int],
      conflictPopup: Option[(Int, Int)],
      draggingRow: Option[Int],
      draggingColumn: Option[Int],
      previewRow: Option[Int],
      previewColumn: Option[Int],
      rangeDragStart: Option[(Int, Int)],
      rangePreviewEnd: Option[(Int, Int)]
  )

  class Backend($ : BackendScope[Props, State]) {

    private val replicaEventPrint: (LocalUid, String) => Callback = (replicaId, msg) => Callback(println(s"[${replicaId.show}]: $msg"))

    private def modSpreadsheet(f: LocalUid ?=> Spreadsheet[String] => Spreadsheet[String]): Callback = {
      $.props.flatMap{ props =>
        given LocalUid = props.replicaId
        val delta      = props.spreadsheetAggregator.editAndGetDelta()(f)
        props.spreadsheetAggregator.visit(_.printToConsole())
        props.onDelta(delta)
      }
    }

    private def withSelectedRow(f: Int => Callback): Callback =
      $.state.flatMap(_.selectedRow.map(f).getOrElse(Callback.empty))

    private def withSelectedColumn(f: Int => Callback): Callback =
      $.state.flatMap(_.selectedColumn.map(f).getOrElse(Callback.empty))

    private def withEditingCell(f: (coordinate: SpreadsheetCoordinate, content: String) => Callback): Callback =
      $.state.flatMap(state => (
        for{
          (rowIdx, colIdx) <- state.editingCell
        } yield SpreadsheetCoordinate(rowIdx, colIdx)
      ).map(f(_, state.editingValue)).getOrElse(Callback.empty))

    private def withSelectedRowAndProps(f: (Int, Props) => Callback): Callback =
      withSelectedRow(rowIdx => $.props.flatMap(props => f(rowIdx, props)))

    private def withSelectedColumnAndProps(f: (Int, Props) => Callback): Callback =
      withSelectedColumn(colIdx => $.props.flatMap(props => f(colIdx, props)))

    private def withEditAndProps(f: (SpreadsheetCoordinate, String, Props) => Callback): Callback =
      withEditingCell((coordinate, content) => $.props.flatMap(props => f(SpreadsheetCoordinate(coordinate.rowIdx, coordinate.colIdx), content, props)))

    def handleDoubleClick(rowIdx: Int, colIdx: Int): Callback =
      concludeEdit()
      >> $.props.flatMap{ props =>
        val currentSet = props.spreadsheetAggregator.current.read(SpreadsheetCoordinate(rowIdx, colIdx))
        val firstValue = currentSet.getFirstOrEmpty.getOrElse("")
        $.modState(_.copy(editingCell = Some((rowIdx, colIdx)), editingValue = firstValue))
        >> modSpreadsheet(_.addRange(props.replicaId.uid, SpreadsheetCoordinate(rowIdx, colIdx), SpreadsheetCoordinate(rowIdx, colIdx)))
      }

    def openConflict(row: Int, col: Int): Callback =
      $.modState(_.copy(conflictPopup = Some((row, col))))

    def closeConflict(): Callback =
      $.modState(_.copy(conflictPopup = None))

    def keepValue(row: Int, col: Int, v: String): Callback =
      modSpreadsheet(_.editCell(SpreadsheetCoordinate(row, col), v)) >> closeConflict()

    def handleInputChange(e: ReactEventFromInput): Callback = {
      val value = e.target.value
      println(s"Edit value: $value")
      $.modState(_.copy(editingValue = value))
    }

    def handleKeyPress(e: ReactKeyboardEventFromInput): Callback = {
      e.key match{
        case "Enter"  => commitEdit()
        case "Escape" => concludeEdit()
        case _        => Callback.empty
      }
    }

    private def commitEdit(): Callback = {
      withEditAndProps{(coordinate, content, _) =>
        var value = content.trim
        if (value.isBlank) value = null
        modSpreadsheet(_.editCell(coordinate, value))
      } >> concludeEdit(successful = true)
    }

    private def concludeEdit(successful: Boolean = false): Callback =
      withEditAndProps{(coordinate, content, props) =>
        replicaEventPrint(props.replicaId, s"Edit at (${coordinate.rowIdx + 1}, ${coordinate.colIdx + 1}) was ${if successful then "committed" else "aborted"}: \"$content\"")
        >> modSpreadsheet(_.removeRange(props.replicaId.uid))
      } >> $.modState(_.copy(editingCell = None, editingValue = ""))

    def handleRangeMouseDown(rowIdx: Int, colIdx: Int): Callback =
      $.modState(_.copy(rangeDragStart = Some((rowIdx, colIdx)), rangePreviewEnd = Some((rowIdx, colIdx))))

    def handleRangeMouseOver(rowIdx: Int, colIdx: Int): Callback =
      $.state.flatMap{ state =>
        state.rangeDragStart
          .map(_ => $.modState(_.copy(rangePreviewEnd = Some((rowIdx, colIdx)))))
          .getOrElse(Callback.empty)
      }

    def handleRangeMouseUp(): Callback = {
      $.state.flatMap{ state =>
        (for {
          start <- state.rangeDragStart
          end   <- state.rangePreviewEnd
        } yield (start, end))
          .map { case ((r1, c1), (r2, c2)) =>
            $.props.flatMap { props =>
              given LocalUid = props.replicaId
              val from       = SpreadsheetCoordinate(math.min(r1, r2), math.min(c1, c2))
              val to         = SpreadsheetCoordinate(math.max(r1, r2), math.max(c1, c2))
              val rangeId    = Uid.gen()
              modSpreadsheet(_.addRange(rangeId, from, to))
            }
          }
          .getOrElse(Callback.empty)
      } >> $.modState(_.copy(rangeDragStart = None, rangePreviewEnd = None))
    }

    def selectRow(rowIdx: Int): Callback = {
      $.modState(_.copy(selectedRow = Some(rowIdx), selectedColumn = None))
    }

    def selectColumn(colIdx: Int): Callback = {
      $.modState(_.copy(selectedColumn = Some(colIdx), selectedRow = None))
    }

    def insertRowAbove(): Callback =
      withSelectedRowAndProps{(rowIdx, props) =>
        replicaEventPrint(props.replicaId, s"Inserting Row Before ${rowIdx + 1}")
        >> concludeEdit()
        >> modSpreadsheet(_.insertRow(rowIdx))
      } >> $.modState(st => st.copy(selectedRow = Option(st.selectedRow.get)))

    def insertRowBelow(): Callback =
      withSelectedRowAndProps{(rowIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if rowIdx == spreadsheet.numRows - 1 then modSpreadsheet(_.addRow())
          else modSpreadsheet(_.insertRow(rowIdx + 1))
        replicaEventPrint(props.replicaId, s"Inserting Row After ${rowIdx + 1}")
        >> concludeEdit()
        >> action
      } >> $.modState(st => st.copy(selectedRow = Option(st.selectedRow.get + 1)))

    def removeRow(): Callback =
      var numRows: Int = 0
      withSelectedRowAndProps{(rowIdx, props) =>
        numRows = props.spreadsheetAggregator.current.numRows
        replicaEventPrint(props.replicaId, s"Removing Row ${rowIdx + 1}")
        >> concludeEdit()
        >> modSpreadsheet(_.removeRow(rowIdx))
        //>> modSpreadsheet(_.purgeTombstones())
      } >> $.modState(st => st.copy(selectedRow = for {o <- Some(numRows-2); if o >= 0} yield o min st.selectedRow.get max 0))

    def insertColumnLeft(): Callback =
      withSelectedColumnAndProps{(colIdx, props) =>
        replicaEventPrint(props.replicaId, s"Inserting Column Before ${colIdx + 1}")
        >> concludeEdit()
        >> modSpreadsheet(_.insertColumn(colIdx))
      } >> $.modState(st => st.copy(selectedColumn = Some(st.selectedColumn.get)))

    def insertColumnRight(): Callback =
      withSelectedColumnAndProps{(colIdx, props) =>
        val spreadsheet = props.spreadsheetAggregator.current
        val action =
          if colIdx == spreadsheet.numColumns - 1 then modSpreadsheet(_.addColumn())
          else modSpreadsheet(_.insertColumn(colIdx + 1))
        replicaEventPrint(props.replicaId, s"Inserting Column After ${colIdx + 1}")
        >> concludeEdit()
        >> action
      } >> $.modState(st => st.copy(selectedColumn = Some(st.selectedColumn.get + 1)))


    def removeColumn(): Callback = {
      var numCols: Int = 0
      withSelectedColumnAndProps{(colIdx, props) =>
        numCols = props.spreadsheetAggregator.current.numColumns
        replicaEventPrint(props.replicaId, s"Removing Column ${colIdx + 1}")
        >> concludeEdit()
        >> modSpreadsheet(_.removeColumn(colIdx))
        //>> modSpreadsheet(_.purgeTombstones())
      } >> $.modState(st => st.copy(selectedColumn = for {o <- Some(numCols-2); if o >= 0} yield o min st.selectedColumn.get max 0))
    }

    def addRow(): Callback =
      concludeEdit()
      >> modSpreadsheet(_.addRow())

    def addColumn(): Callback =
      concludeEdit()
      >> modSpreadsheet(_.addColumn())

    // TODO: current replicated list does not allow purging
    def purgeTombstones(): Callback = modSpreadsheet(identity)

    def handleRowDragStart(rowIdx: Int): Callback =
      $.modState(_.copy(draggingRow = Some(rowIdx), previewRow = None, previewColumn = None))

    def handleRowDrop(targetIdx: Int)(e: ReactDragEvent): Callback = {
      $.state.flatMap { st =>
        val insertionIdx = st.previewRow.get
        st.draggingRow
          .map { srcIdx =>
            $.props.flatMap(props =>
              replicaEventPrint(props.replicaId, s"Dragged row $srcIdx to index $insertionIdx")
              >> concludeEdit()
              >> modSpreadsheet(_.moveRow(srcIdx, insertionIdx))
            )
          }
          .getOrElse(Callback.empty)
      } >> $.modState(_.copy(draggingRow = None, previewRow = None))
    }

    def handleRowDragOver(targetIdx: Int)(e: ReactDragEvent): Callback = {
      val elem  = e.currentTarget.asInstanceOf[dom.html.Element]
      val rect  = elem.getBoundingClientRect()
      val isAbove = e.clientY < (rect.top + rect.height / 2)
      val insertionIdx = if isAbove then targetIdx else targetIdx + 1
      Callback(e.preventDefault()) >> $.modState(_.copy(previewRow = Some(insertionIdx)))
    }

    def handleColumnDragStart(colIdx: Int): Callback =
      $.modState(_.copy(draggingColumn = Some(colIdx), previewColumn = None, previewRow = None))

    def handleColumnDrop(targetIdx: Int)(e: ReactDragEvent): Callback = {
      $.state.flatMap { st =>
        val insertionIdx = st.previewColumn.get
        st.draggingColumn
          .map { srcIdx =>
            $.props.flatMap(props =>
              replicaEventPrint(props.replicaId, s"Dragged column $srcIdx to index $insertionIdx")
              >> concludeEdit()
              >> modSpreadsheet(_.moveColumn(srcIdx, insertionIdx))
            )
          }
          .getOrElse(Callback.empty)
      } >> $.modState(_.copy(draggingColumn = None, previewColumn = None))
    }

    def handleColumnDragOver(targetIdx: Int)(e: ReactDragEvent): Callback = {
      val elem  = e.currentTarget.asInstanceOf[dom.html.Element]
      val rect  = elem.getBoundingClientRect()
      val isLeft = e.clientX < (rect.left + rect.width / 2)
      val insertionIdx = if isLeft then targetIdx else targetIdx + 1
      Callback(e.preventDefault()) >> $.modState(_.copy(previewColumn = Some(insertionIdx)))
    }

    def handleDragOver(e: ReactDragEvent): Callback = Callback(e.preventDefault())

    // Delete a range by its id
    def deleteRange(rangeId: Uid): Callback =
      concludeEdit() >> modSpreadsheet(_.removeRange(rangeId))
  }

  private def onHold(ms: Int)(cb: Callback): TagMod =
  {
    var timerActive: Option[SetTimeoutHandle] = None
    TagMod(
      ^.onMouseDown --> Callback {
        timerActive.foreach(timers.clearTimeout)
        timerActive =
          Some(timers.setTimeout(ms){
              cb.runNow()
              timerActive = None
          })
      },
      ^.onMouseUp --> Callback {
        timerActive.foreach(timers.clearTimeout); timerActive = None
      },
      ^.onMouseLeave --> Callback {
        timerActive.foreach(timers.clearTimeout); timerActive = None
      }
    )
  }

  val Component: Component[Props, State, Backend, Aux[Box[Props], Children.None, CtorType.Props]#CT] = ScalaComponent
    .builder[Props]("Spreadsheet")
    .initialState(State(None, "", None, None, None, None, None, None, None, None, None))
    .backend(new Backend(_))
    .render { $ =>
      val props       = $.props
      val state       = $.state
      val backend     = $.backend
      val spreadsheet = props.spreadsheetAggregator.current
      val data        = spreadsheet.toList

      val allRangesWithIds = spreadsheet.listRangesWithIds

      val palette = List("red", "orange", "yellow", "lime", "green", "teal", "cyan", "sky", "blue", "indigo", "purple", "pink")
      def bgClass(idx: Int): String     = s"bg-${palette(idx % palette.length)}-100"
      def borderClass(idx: Int): String = s"border-${palette(idx % palette.length)}-500"

      def colorIndexForId(id: Uid): Int = id.show.hashCode.abs % palette.length

      val previewRangeOpt = for {
        start <- state.rangeDragStart
        end   <- state.rangePreviewEnd
      } yield (start, end)

      def inside(range: com.daimpl.lib.Spreadsheet.Range, r: Int, c: Int): Boolean =
        val minRow = math.min(range.from.rowIdx, range.to.rowIdx)
        val maxRow = math.max(range.from.rowIdx, range.to.rowIdx)
        val minCol = math.min(range.from.colIdx, range.to.colIdx)
        val maxCol = math.max(range.from.colIdx, range.to.colIdx)
        r >= minRow && r <= maxRow && c >= minCol && c <= maxCol

      def onBoundary(range: com.daimpl.lib.Spreadsheet.Range, r: Int, c: Int): (Boolean, Boolean, Boolean, Boolean) = {
        val minRow = math.min(range.from.rowIdx, range.to.rowIdx)
        val maxRow = math.max(range.from.rowIdx, range.to.rowIdx)
        val minCol = math.min(range.from.colIdx, range.to.colIdx)
        val maxCol = math.max(range.from.colIdx, range.to.colIdx)
        (
          r == minRow && c >= minCol && c <= maxCol, // top
          r == maxRow && c >= minCol && c <= maxCol, // bottom
          c == minCol && r >= minRow && r <= maxRow, // left
          c == maxCol && r >= minRow && r <= maxRow  // right
        )
      }

      def cellStyleClasses(rIdx: Int, cIdx: Int): String = {
        var background  = ""
        var borderT     = ""
        var borderB     = ""
        var borderL     = ""
        var borderR     = ""

        allRangesWithIds.foreach { case (rid, rng) =>
          val idx = colorIndexForId(rid)
          if inside(rng, rIdx, cIdx) then {
            if background.isEmpty then background = s" ${bgClass(idx)}"

            val (isTop, isBottom, isLeft, isRight) = onBoundary(rng, rIdx, cIdx)
            val colCls = borderClass(idx)
            borderT = s" border-t-${if isTop    then 2 else 1} $colCls"
            borderB = s" border-b-${if isBottom then 2 else 1} $colCls"
            borderL = s" border-l-${if isLeft   then 2 else 1} $colCls"
            borderR = s" border-r-${if isRight  then 2 else 1} $colCls"
          }
        }

        previewRangeOpt.foreach { case ((r1, c1), (r2, c2)) =>
          val minRow = math.min(r1, r2)
          val maxRow = math.max(r1, r2)
          val minCol = math.min(c1, c2)
          val maxCol = math.max(c1, c2)
          if rIdx >= minRow && rIdx <= maxRow && cIdx >= minCol && cIdx <= maxCol then {
            background = " bg-yellow-50"
            borderT = s" border-t-${if rIdx == minRow then 2 else 1} border-yellow-400 border-dashed"
            borderB = s" border-b-${if rIdx == maxRow then 2 else 1} border-yellow-400 border-dashed"
            borderL = s" border-l-${if cIdx == minCol then 2 else 1} border-yellow-400 border-dashed"
            borderR = s" border-r-${if cIdx == maxCol then 2 else 1} border-yellow-400 border-dashed"
          }
        }

        background + borderT + borderB + borderL + borderR
      }

      <.div(
        ^.onMouseUp --> backend.handleRangeMouseUp(),
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
                  "#",
                  ^.onDragOver ==> backend.handleColumnDragOver(-1),
                  ^.onDrop ==> backend.handleColumnDrop(-1)
                ),
                (0 until spreadsheet.numColumns)
                  .map(i =>
                    <.th(
                      ^.key       := s"header-$i",
                      ^.className := {
                        val baseClass =
                          "border border-gray-300 px-4 py-2 font-semibold w-32 max-w-32 cursor-pointer hover:bg-gray-200"
                        val selectedClass = if state.selectedColumn.contains(i) then " bg-blue-200" else " bg-gray-100"
                        val previewLeft  = if state.previewColumn.contains(i) then " border-l-4 border-blue-400" else ""
                        val previewRight = if state.previewColumn.contains(i + 1) && i == spreadsheet.numColumns - 1 then " border-r-4 border-blue-400" else ""
                        baseClass + selectedClass + previewLeft + previewRight
                      },
                      ^.onClick --> backend.selectColumn(i),
                      ^.draggable := true,
                      ^.onDragStart --> backend.handleColumnDragStart(i),
                      ^.onDragOver ==> backend.handleColumnDragOver(i),
                      ^.onDrop ==> backend.handleColumnDrop(i),
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
                      val selectedClass = if state.selectedRow.contains(rowIdx) then " bg-blue-200" else " bg-gray-50"
                      val previewTop    = if state.previewRow.contains(rowIdx) then " border-t-4 border-blue-400" else ""
                      val previewBottom = if state.previewRow.contains(rowIdx + 1) && rowIdx == spreadsheet.numRows - 1 then " border-b-4 border-blue-400" else ""
                      baseClass + selectedClass + previewTop + previewBottom
                    },
                    ^.onClick --> backend.selectRow(rowIdx),
                    ^.draggable := true,
                    ^.onDragStart --> backend.handleRowDragStart(rowIdx),
                    ^.onDragOver ==> backend.handleRowDragOver(rowIdx),
                    ^.onDrop ==> backend.handleRowDrop(rowIdx),
                    ^.title := "Click to select row",
                    (rowIdx + 1).toString
                  ),
                  row.zipWithIndex.map { case (cell, colIdx) =>
                    <.td(
                      ^.key       := s"cell-$rowIdx-$colIdx",
                      ^.className := {
                        val baseClass = "border border-gray-300 px-4 py-2 text-center relative w-32 max-w-32"
                        baseClass + cellStyleClasses(rowIdx, colIdx)
                      },
                      ^.onDoubleClick --> backend.handleDoubleClick(rowIdx, colIdx),
                      onHold(200)(backend.handleRangeMouseDown(rowIdx, colIdx)),
                      ^.onMouseOver --> backend.handleRangeMouseOver(rowIdx, colIdx),
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
                            ^.onKeyDown ==> backend.handleKeyPress,
                            ^.className := "w-full h-full border-none outline-none px-2 py-1 max-w-full",
                            ^.autoFocus := true
                          )
                        case _ =>
                          <.span(
                            ^.className := "block cursor-pointer min-h-[1.5rem] overflow-hidden text-ellipsis whitespace-nowrap",
                            ^.title := cell.formatConflicts(),
                            cell.formatConflicts()
                          )
                      },
                      {
                        val startingRanges = allRangesWithIds.filter { case (_, rng) =>
                          val minRow = math.min(rng.from.rowIdx, rng.to.rowIdx)
                          val maxCol = math.max(rng.from.colIdx, rng.to.colIdx)
                          minRow == rowIdx && maxCol == colIdx
                        }
                        startingRanges.zipWithIndex.map { case ((rid, rng), btnIdx) =>
                          val idxColor = colorIndexForId(rid)
                          <.span(
                            ^.key := s"del-${rid.show}",
                            ^.className := s"absolute top-${btnIdx * 1.25} right-0 mr-0.5 mt-0.5 text-${palette(idxColor % palette.length)}-600 cursor-pointer text-xs select-none",
                            ^.title := "Delete range",
                            "✕",
                            ^.onMouseDown ==> ((e: ReactMouseEvent) => e.stopPropagationCB),
                            ^.onClick ==> ((e: ReactMouseEvent) => e.stopPropagationCB >> backend.deleteRange(rid))
                          )
                        }.toVdomArray
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
            val vals = props.spreadsheetAggregator.current.read(SpreadsheetCoordinate(r, c)).toList
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
