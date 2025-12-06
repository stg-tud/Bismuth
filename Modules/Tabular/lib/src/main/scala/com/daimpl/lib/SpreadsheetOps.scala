package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.{RangeId, SpreadsheetCoordinate}
import rdts.base.LocalUid
import rdts.time.Dot

case class RowResult[A](newRowId: Dot, delta: Spreadsheet[A])
case class ColumnResult[A](newColumnId: Dot, delta: Spreadsheet[A])

trait SpreadsheetOps[A] {
  def addRow()(using LocalUid): RowResult[A]

  def addColumn()(using LocalUid): ColumnResult[A]

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[A]

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A]

  def insertRow(rowIdx: Int)(using LocalUid): RowResult[A]

  def insertColumn(colIdx: Int)(using LocalUid): ColumnResult[A]

  def moveRow(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A]

  def moveColumn(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A]

  def editCell(coordinate: SpreadsheetCoordinate, value: A | Null)(using LocalUid): Spreadsheet[A]

  def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[A]

  def removeRange(id: RangeId): Spreadsheet[A]

  def purgeTombstones: Spreadsheet[A]
}
