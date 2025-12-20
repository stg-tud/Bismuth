package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.SpreadsheetCoordinate
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

  override def removeRow(rowIdx: RowIndex)(using LocalUid): Spreadsheet[S] = {
    val undo = delegate.internal.keepRow(rowIdx)
    pushUndo { s => s `merge` undo }
    delegate.removeRow(rowIdx)
  }

  override def removeColumn(colIdx: ColumnIndex)(using LocalUid): Spreadsheet[S] = {
    val undo = delegate.internal.keepColumn(colIdx)
    pushUndo { s => s `merge` undo }
    delegate.removeColumn(colIdx)
  }

  override def insertRow(rowIdx: RowIndex)(using LocalUid): RowResult[S] = {
    val res = delegate.insertRow(rowIdx)
    pushUndo { s => s.removeRowById(res.newRowId) }
    res
  }

  override def insertColumn(colIdx: ColumnIndex)(using LocalUid): ColumnResult[S] = {
    val res = delegate.insertColumn(colIdx)
    pushUndo { s => s.removeColumnById(res.newColumnId) }
    res
  }

  override def moveRow(sourceIdx: RowIndex, targetIdx: RowIndex)(using LocalUid): Spreadsheet[S] = {
    val forwardSourceRowIdOpt = delegate.getRowId(sourceIdx)
    val forwardTargetRowIdOpt = delegate.getRowId(targetIdx)

    if (forwardSourceRowIdOpt.isDefined && forwardTargetRowIdOpt.isDefined) {
      pushUndo { s =>
        val forwardSourceRowIdxOpt = delegate.getRowIndex(forwardSourceRowIdOpt.get)
        val forwardTargetRowIdxOpt = delegate.getRowIndex(forwardTargetRowIdOpt.get)

        val targetToSourceIdx = (targetIdx: RowIndex, sourceIdx: RowIndex) => if (sourceIdx < targetIdx) (targetIdx - 1).toRowIndex else targetIdx

        val inverseTargetRowIdx = forwardSourceRowIdxOpt.getOrElse(sourceIdx)

        if (forwardTargetRowIdxOpt.isDefined) {
          val inverseSourceRowIdx = targetToSourceIdx(inverseTargetRowIdx, forwardTargetRowIdxOpt.get)
          s.moveRow(inverseSourceRowIdx, inverseTargetRowIdx)
        }
        else Spreadsheet.empty[S]
      }
    }

    delegate.moveRow(sourceIdx, targetIdx)
  }

  override def moveColumn(sourceIdx: ColumnIndex, targetIdx: ColumnIndex)(using LocalUid): Spreadsheet[S] = {
    val forwardSourceColIdOpt = delegate.getColId(sourceIdx)
    val forwardTargetColIdOpt = delegate.getColId(targetIdx)

    if (forwardSourceColIdOpt.isDefined && forwardTargetColIdOpt.isDefined) {
      pushUndo { s =>
        val forwardSourceColIdxOpt = delegate.getColIndex(forwardSourceColIdOpt.get)
        val forwardTargetColIdxOpt = delegate.getColIndex(forwardTargetColIdOpt.get)

        val targetToSourceIdx = (targetIdx: ColumnIndex, sourceIdx: ColumnIndex) => if (sourceIdx < targetIdx) (targetIdx - 1).toColumnIndex else targetIdx

        val inverseTargetColIdx = forwardSourceColIdxOpt.getOrElse(sourceIdx)

        if (forwardTargetColIdxOpt.isDefined) {
          val inverseSourceColIdx = targetToSourceIdx(inverseTargetColIdx, forwardTargetColIdxOpt.get)
          s.moveColumn(inverseSourceColIdx, inverseTargetColIdx)
        }
        else Spreadsheet.empty[S]
      }
    }

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
    val before = delegate.getRange(id)
    if (before.isDefined) {
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
