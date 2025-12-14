package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.SpreadsheetCoordinate
import rdts.base.{LocalUid, Uid}
import rdts.time.Dot

import scala.annotation.targetName

case class RowResult[A](newRowId: RowId, delta: Spreadsheet[A])
case class ColumnResult[A](newColumnId: ColumnId, delta: Spreadsheet[A])

opaque type RangeId = Uid
opaque type RowId <: Dot = Dot
opaque type ColumnId <: Dot = Dot

object RangeId {
  def gen: RangeId = Uid.gen()
  
  def apply(id: Uid): RangeId = id
}

opaque type RowIndex <: Int = Int
opaque type ColumnIndex <: Int = Int

extension (index: ColumnIndex)
  def withOffset(offset: Int): ColumnIndex = index + offset


extension (index: RowIndex)
  @targetName("plusRow")
  def withOffset(offset: Int): RowIndex = index + offset

extension (raw: Int)
  def toRowIndex: RowIndex = raw
  def toColumnIndex: ColumnIndex = raw


extension (raw: Dot)
  def toRowId: RowId = raw
  def toColumnId: ColumnId = raw

trait SpreadsheetOps[A] {
  def addRow()(using LocalUid): RowResult[A]

  def addColumn()(using LocalUid): ColumnResult[A]

  def removeRow(rowIdx: RowIndex)(using LocalUid): Spreadsheet[A]

  def removeColumn(colIdx: ColumnIndex)(using LocalUid): Spreadsheet[A]

  def insertRow(rowIdx: RowIndex)(using LocalUid): RowResult[A]

  def insertColumn(colIdx: ColumnIndex)(using LocalUid): ColumnResult[A]

  def moveRow(sourceIdx: RowIndex, targetIdx: RowIndex)(using LocalUid): Spreadsheet[A]

  def moveColumn(sourceIdx: ColumnIndex, targetIdx: ColumnIndex)(using LocalUid): Spreadsheet[A]

  def editCell(coordinate: SpreadsheetCoordinate, value: A | Null)(using LocalUid): Spreadsheet[A]

  def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[A]

  def removeRange(id: RangeId): Spreadsheet[A]

  def purgeTombstones: Spreadsheet[A]
}
