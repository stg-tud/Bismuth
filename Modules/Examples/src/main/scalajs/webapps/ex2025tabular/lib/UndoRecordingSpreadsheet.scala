package webapps.ex2025tabular.lib

import rdts.base.ReplicaId
import webapps.ex2025tabular.lib.Spreadsheet.SpreadsheetCoordinate

class UndoRecordingSpreadsheet[S](
    val delegate: Spreadsheet[S],
    pushUndo: (ReplicaId ?=> Spreadsheet[S] => Spreadsheet[S]) => Unit
) extends SpreadsheetOps[S] {
  override def addRow()(using ReplicaId): RowResult[S] = {
    val res = delegate.addRow()
    pushUndo { s => s.removeRowById(res.newRowId) }
    res
  }

  override def addColumn()(using ReplicaId): ColumnResult[S] = {
    val res = delegate.addColumn()
    pushUndo { s => s.removeColumnById(res.newColumnId) }
    res
  }

  override def removeRow(rowIdx: RowIndex)(using ReplicaId): Spreadsheet[S] = {
    val undo = delegate.internal.keepRow(rowIdx)
    val id   = delegate.getRowId(rowIdx).get
    pushUndo { s => if !s.listRowIds.contains(id) then s `merge` undo else s }
    delegate.removeRow(rowIdx)
  }

  override def removeColumn(colIdx: ColumnIndex)(using ReplicaId): Spreadsheet[S] = {
    val undo = delegate.internal.keepColumn(colIdx)
    val id   = delegate.getColId(colIdx).get
    pushUndo { s => if !s.listColumnIds.contains(id) then s `merge` undo else s }
    delegate.removeColumn(colIdx)
  }

  override def insertRow(rowIdx: RowIndex)(using ReplicaId): RowResult[S] = {
    val res = delegate.insertRow(rowIdx)
    pushUndo { s => s.removeRowById(res.newRowId) }
    res
  }

  override def insertColumn(colIdx: ColumnIndex)(using ReplicaId): ColumnResult[S] = {
    val res = delegate.insertColumn(colIdx)
    pushUndo { s => s.removeColumnById(res.newColumnId) }
    res
  }

  override def moveRow(sourceIdx: RowIndex, targetIdx: RowIndex)(using ReplicaId): Spreadsheet[S] = {
    pushUndo { s => s.moveRow(if sourceIdx < targetIdx then (targetIdx - 1).toRowIndex else targetIdx, sourceIdx) }
    delegate.moveRow(sourceIdx, targetIdx)
  }

  override def moveColumn(sourceIdx: ColumnIndex, targetIdx: ColumnIndex)(using ReplicaId): Spreadsheet[S] = {
    pushUndo { s =>
      s.moveColumn(if sourceIdx < targetIdx then (targetIdx - 1).toColumnIndex else targetIdx, sourceIdx)
    }
    delegate.moveColumn(sourceIdx, targetIdx)
  }

  override def editCell(coordinate: SpreadsheetCoordinate, value: Option[S], solveSeenConflict: Boolean = true)(using
      ReplicaId
  ): Spreadsheet[S] = {
    val rowIdOpt = delegate.getRowId(coordinate.rowIdx)
    val colIdOpt = delegate.getColId(coordinate.colIdx)

    if rowIdOpt.isDefined && colIdOpt.isDefined then {
      val previousValues = delegate.read(coordinate).toList
      pushUndo { s =>
        val removeNewValueDelta = value match
            case None    => Spreadsheet.empty[S]
            case Some(v) => s.internal.removeValueFromConflict(rowIdOpt.get, colIdOpt.get, v)

        previousValues.foldLeft(removeNewValueDelta) { (accDelta, previousValue) =>
          accDelta.merge(s.editCellById(rowIdOpt.get, colIdOpt.get, Some(previousValue), solveSeenConflict = false))
        }
      }
    }

    delegate.editCell(coordinate, value)
  }

  override def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using
      ReplicaId
  ): Spreadsheet[S] = {
    val before = delegate.getRange(id)
    if before.isDefined then {
      pushUndo { s => s.addRange(id, before.get.from, before.get.to) }
    } else {
      pushUndo { s => s.removeRange(id) }
    }
    delegate.addRange(id, from, to)
  }

  override def removeRange(id: RangeId): Spreadsheet[S] = {
    delegate.getRange(id) match {
      case Some(range) =>
        pushUndo { s => s.addRange(id, range.from, range.to) }
      case None =>
    }
    delegate.removeRange(id)
  }

  override def purgeTombstones: Spreadsheet[S] = delegate.purgeTombstones
}
