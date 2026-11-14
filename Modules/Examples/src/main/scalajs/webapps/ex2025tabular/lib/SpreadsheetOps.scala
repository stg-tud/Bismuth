package webapps.ex2025tabular.lib

import channels.JsoniterCodecs
import channels.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonValueCodec}
import rdts.base.{ReplicaId, Uid}
import rdts.time.Dot
import webapps.ex2025tabular.lib.Spreadsheet.SpreadsheetCoordinate

import scala.annotation.targetName

case class RowResult[A](newRowId: RowId, delta: Spreadsheet[A])
case class ColumnResult[A](newColumnId: ColumnId, delta: Spreadsheet[A])

opaque type RangeId         = Uid
opaque type RowId <: Dot    = Dot
opaque type ColumnId <: Dot = Dot

object RangeId {
  def gen: RangeId = Uid.gen()

  def apply(id: Uid): RangeId = id
}

/** Codecs for the opaque ids (which are just a [[rdts.time.Dot]] or [[rdts.base.Uid]] at runtime). */
object IdCodecs {
  given rowIdCodec: JsonValueCodec[RowId]        = JsoniterCodecs.dotCodec.asInstanceOf[JsonValueCodec[RowId]]
  given columnIdCodec: JsonValueCodec[ColumnId]  = JsoniterCodecs.dotCodec.asInstanceOf[JsonValueCodec[ColumnId]]
  given rangeIdCodec: JsonValueCodec[RangeId]    = summon[JsonValueCodec[Uid]].asInstanceOf[JsonValueCodec[RangeId]]
  given rowIdKeyCodec: JsonKeyCodec[RowId]       = JsoniterCodecs.dotKeyCodec.asInstanceOf[JsonKeyCodec[RowId]]
  given columnIdKeyCodec: JsonKeyCodec[ColumnId] = JsoniterCodecs.dotKeyCodec.asInstanceOf[JsonKeyCodec[ColumnId]]
  given rangeIdKeyCodec: JsonKeyCodec[RangeId]   = JsoniterCodecs.uidKeyCodec.asInstanceOf[JsonKeyCodec[RangeId]]
}

opaque type RowIndex <: Int    = Int
opaque type ColumnIndex <: Int = Int

extension (index: ColumnIndex)
    def withOffset(offset: Int): ColumnIndex = index + offset

extension (index: RowIndex)
    @targetName("plusRow")
    def withOffset(offset: Int): RowIndex = index + offset

extension (raw: Int)
    def toRowIndex: RowIndex       = raw
    def toColumnIndex: ColumnIndex = raw

extension (raw: Dot)
    def toRowId: RowId       = raw
    def toColumnId: ColumnId = raw

trait SpreadsheetOps[A] {
  def addRow()(using ReplicaId): RowResult[A]

  def addColumn()(using ReplicaId): ColumnResult[A]

  def removeRow(rowIdx: RowIndex)(using ReplicaId): Spreadsheet[A]

  def removeColumn(colIdx: ColumnIndex)(using ReplicaId): Spreadsheet[A]

  def insertRow(rowIdx: RowIndex)(using ReplicaId): RowResult[A]

  def insertColumn(colIdx: ColumnIndex)(using ReplicaId): ColumnResult[A]

  def moveRow(sourceIdx: RowIndex, targetIdx: RowIndex)(using ReplicaId): Spreadsheet[A]

  def moveColumn(sourceIdx: ColumnIndex, targetIdx: ColumnIndex)(using ReplicaId): Spreadsheet[A]

  def editCell(coordinate: SpreadsheetCoordinate, value: Option[A], solveSeenConflict: Boolean = true)(using
      ReplicaId
  ): Spreadsheet[A]

  def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using ReplicaId): Spreadsheet[A]

  def removeRange(id: RangeId): Spreadsheet[A]

  def purgeTombstones: Spreadsheet[A]
}
