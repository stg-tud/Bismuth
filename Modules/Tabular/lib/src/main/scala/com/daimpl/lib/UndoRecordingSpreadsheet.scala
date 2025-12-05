package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.{RangeId, SpreadsheetCoordinate}
import rdts.base.LocalUid

class UndoRecordingSpreadsheet[S](
 val delegate: Spreadsheet[S],
 pushUndo: (LocalUid ?=> Spreadsheet[S] => Spreadsheet[S]) => Unit
) extends SpreadsheetOps[S] {
  override def addRow()(using LocalUid): RowResult[S] = {
    val res = delegate.addRow()
    pushUndo { s => s.removeRowById(res.newRowId) }
    res
  }

  override def addColumn()(using LocalUid): ColumnResult[S] = {
    val res = delegate.addColumn()
    pushUndo { s => s.removeColumnById(res.newColumnId) }
    res
  }

  override def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[S] = {
    pushUndo { s => s.insertRow(rowIdx).delta }
    delegate.removeRow(rowIdx)
  }

  override def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[S] = {
    pushUndo { s => s.insertColumn(colIdx).delta }
    delegate.removeColumn(colIdx)
  }

  override def insertRow(rowIdx: Int)(using LocalUid): RowResult[S] = {
    val res = delegate.insertRow(rowIdx)
    pushUndo { s => s.removeRowById(res.newRowId) }
    res
  }

  override def insertColumn(colIdx: Int)(using LocalUid): ColumnResult[S] = {
    val res = delegate.insertColumn(colIdx)
    pushUndo { s => s.removeColumnById(res.newColumnId) }
    res
  }

  override def moveRow(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[S] = {
    pushUndo { s => s.moveRow(targetIdx, sourceIdx) }
    delegate.moveRow(sourceIdx, targetIdx)
  }

  override def moveColumn(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[S] = {
    pushUndo { s => s.moveColumn(targetIdx, sourceIdx) }
    delegate.moveColumn(sourceIdx, targetIdx)
  }

  override def editCell(coordinate: SpreadsheetCoordinate, value: S | Null)(using LocalUid): Spreadsheet[S] = {
    val rowIdOpt = delegate.getRowId(coordinate.rowIdx)
    val colIdOpt = delegate.getColId(coordinate.colIdx)

    if (rowIdOpt.isDefined && colIdOpt.isDefined) {
      val currentVal = delegate.read(coordinate).elements.headOption.orNull
      pushUndo { s => s.editCellById(rowIdOpt.get, colIdOpt.get, currentVal) }
    }

    delegate.editCell(coordinate, value)
  }

  override def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[S] = {
    pushUndo { s => s.removeRange(id) }
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
