package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.MarkerRemovalBehavior
import com.daimpl.lib.Spreadsheet.{Range, SpreadsheetCoordinate}
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import rdts.time.{Dot, Dots}

case class Spreadsheet[A](
    private val rowIds: ReplicatedUniqueList[RowId] = ReplicatedUniqueList.empty[RowId],
    private val colIds: ReplicatedUniqueList[ColumnId] = ReplicatedUniqueList.empty[ColumnId],
    private val rowAndColIdPairToContent: ObserveRemoveMap[(rowId: RowId, colId: ColumnId), ReplicatedSet[A]] =
      ObserveRemoveMap.empty[(rowId: RowId, colId: ColumnId), ReplicatedSet[A]],
    private val rangeIds: ReplicatedSet[RangeId] = ReplicatedSet.empty[RangeId]
) extends SpreadsheetOps[A] {
  lazy val observed: Dots =
    Dots.from(rowIds.toList)
    `union` Dots.from(colIds.toList)

  private def newRowOrColId(using LocalUid): Dot = observed.nextDot(LocalUid.replicaId)

  class SpreadsheetInternal {
    def keepRow(index: RowIndex)(using LocalUid): Spreadsheet[A] = Spreadsheet(
      rowIds = rowIds.insertAt(index, getRowId(index).get)
    )

    def keepColumn(index: ColumnIndex)(using LocalUid): Spreadsheet[A] = Spreadsheet(
      colIds = colIds.insertAt(index, getColId(index).get)
    )
  }

  lazy val internal = SpreadsheetInternal()

  def addRow()(using LocalUid): RowResult[A] =
      val id = newRowOrColId.toRowId
      RowResult(id, Spreadsheet[A](rowIds = rowIds.append(id)))

  def addColumn()(using LocalUid): ColumnResult[A] =
      val id = newRowOrColId.toColumnId
      ColumnResult(id, Spreadsheet[A](colIds = colIds.append(id)))

  def removeRow(rowIdx: RowIndex)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](rowIds = rowIds.removeAt(rowIdx))

  def removeColumn(colIdx: ColumnIndex)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](colIds = colIds.removeAt(colIdx))

  def insertRow(rowIdx: RowIndex)(using LocalUid): RowResult[A] =
      val id = newRowOrColId.toRowId
      RowResult(id, Spreadsheet[A](rowIds = rowIds.insertAt(rowIdx, id)))

  def insertColumn(colIdx: ColumnIndex)(using LocalUid): ColumnResult[A] =
      val id = newRowOrColId.toColumnId
      ColumnResult(id, Spreadsheet[A](colIds = colIds.insertAt(colIdx, id)))

  def moveRow(sourceIdx: RowIndex, targetIdx: RowIndex)(using LocalUid): Spreadsheet[A] =
      val touchedRanges = listRangesWithIds.filter(_._2.touchedRows(sourceIdx))
      val rangeIds      =
        touchedRanges.foldLeft(ReplicatedSet.empty[RangeId]) { (accumulator, rangeAndId) =>
          accumulator.merge({
            val (rangeId, range) = rangeAndId
            if range.validAfterSwapping(sourceIdx, targetIdx) then keepRange(rangeId)
            else removeRange(rangeId)
          }.rangeIds)
        }
      Spreadsheet[A](
        rowIds = rowIds.move(sourceIdx, targetIdx),
        rangeIds = rangeIds
      )

  def moveColumn(sourceIdx: ColumnIndex, targetIdx: ColumnIndex)(using LocalUid): Spreadsheet[A] =
      val touchedRanges = listRangesWithIds.filter(_._2.touchedCols(sourceIdx))
      val rangeIds      =
        touchedRanges.foldLeft(ReplicatedSet.empty[RangeId]) { (accumulator, rangeAndId) =>
          accumulator.merge({
            val (rangeId, range) = rangeAndId
            if range.validAfterSwapping(sourceIdx, targetIdx) then keepRange(rangeId)
            else removeRange(rangeId)
          }.rangeIds)
        }
      Spreadsheet[A](
        colIds = colIds.move(sourceIdx, targetIdx),
        rangeIds = rangeIds
      )

  def editCell(coordinate: SpreadsheetCoordinate, value: A | Null)(using LocalUid): Spreadsheet[A] = {
    val rowId      = rowIds.readAt(coordinate.rowIdx).get
    val colId      = colIds.readAt(coordinate.colIdx).get
    val newContent =
      rowAndColIdPairToContent.transform(rowId, colId) {
        if value == null then
            case None      => None
            case Some(set) => Some(set.clear())
        else
            case None      => Some(ReplicatedSet.empty.add(value.asInstanceOf[A]))
            case Some(set) => Some(set.removeBy(_ != value) `merge` set.add(value.asInstanceOf[A]))
      }
    Spreadsheet[A](
      rowIds = rowIds.updateAt(coordinate.rowIdx, rowId),
      colIds = colIds.updateAt(coordinate.colIdx, colId),
      rowAndColIdPairToContent = newContent
    )
  }

  def purgeTombstones: Spreadsheet[A] = {
    val rows = rowIds.toList
    val cols = colIds.toList
    Spreadsheet[A](rowAndColIdPairToContent = rowAndColIdPairToContent.removeBy {
      idCoord => !(rows.contains(idCoord.rowId) && cols.contains(idCoord.colId))
    })
  }

  def numRows: Int    = rowIds.size
  def numColumns: Int = colIds.size

  def listRowIds: List[RowId]       = rowIds.toList
  def listColumnIds: List[ColumnId] = colIds.toList

  private def getRow(rowIdx: RowIndex): List[ConflictableValue[A]] =
    (0 until numColumns).map(colIdx => read(SpreadsheetCoordinate(rowIdx, colIdx.toColumnIndex))).toList

  def toList: List[List[ConflictableValue[A]]] =
    (0 until numRows).map(toRowIndex).map(getRow).toList

  def read(coordinate: SpreadsheetCoordinate): ConflictableValue[A] =
    (for
        rowId <- rowIds.readAt(coordinate.rowIdx)
        colId <- colIds.readAt(coordinate.colIdx)
        cell  <- rowAndColIdPairToContent.get(rowId, colId)
    yield ConflictableValue(cell.elements)).getOrElse(ConflictableValue.empty[A])

  def getRowId(idx: RowIndex): Option[RowId]       = rowIds.readAt(idx)
  def getColId(idx: ColumnIndex): Option[ColumnId] = colIds.readAt(idx)

  def getRowIndex(id: RowId): Option[RowIndex] = {
    val idx = rowIds.toList.indexOf(id)
    if idx >= 0 then Some(idx.asInstanceOf[RowIndex]) else None
  }

  def getColIndex(id: ColumnId): Option[ColumnIndex] = {
    val idx = colIds.toList.indexOf(id).toColumnIndex
    if idx >= 0 then Some(idx) else None
  }

  def removeRowById(id: RowId)(using LocalUid): Spreadsheet[A] =
    getRowIndex(id) match
        case Some(idx) => removeRow(idx)
        case None      => this

  def removeColumnById(id: ColumnId)(using LocalUid): Spreadsheet[A] =
    getColIndex(id) match
        case Some(idx) => removeColumn(idx)
        case None      => this

  def editCellById(rowId: RowId, colId: ColumnId, value: A | Null)(using LocalUid): Spreadsheet[A] =
    (getRowIndex(rowId), getColIndex(colId)) match
        case (Some(rIdx), Some(cIdx)) =>
          editCell(SpreadsheetCoordinate(rIdx, cIdx), value)
        case _ =>
          this

  def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[A] =
      val idFrom = Uid(id.asInstanceOf[Uid].show + ":from")
      val idTo   = Uid(id.asInstanceOf[Uid].show + ":to")
      Spreadsheet[A](
        rowIds = rowIds.addMarker(idFrom, from.rowIdx, MarkerRemovalBehavior.Successor)
          `merge` rowIds.addMarker(idTo, to.rowIdx, MarkerRemovalBehavior.Predecessor),
        colIds = colIds.addMarker(idFrom, from.colIdx, MarkerRemovalBehavior.Successor)
          `merge` colIds.addMarker(idTo, to.colIdx, MarkerRemovalBehavior.Predecessor),
        rangeIds = rangeIds.add(id)
      )

  def keepRange(id: RangeId)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](rangeIds = rangeIds.add(id))

  def removeRange(id: RangeId): Spreadsheet[A] =
      val idFrom = Uid(id.asInstanceOf[Uid].show + ":from")
      val idTo   = Uid(id.asInstanceOf[Uid].show + ":to")
      Spreadsheet[A](
        rowIds = rowIds.removeMarker(idFrom) `merge` rowIds.removeMarker(idTo),
        colIds = colIds.removeMarker(idFrom) `merge` colIds.removeMarker(idTo),
        rangeIds = rangeIds.remove(id)
      )

  def getRange(id: RangeId): Option[Range] =
      val idFrom = Uid(id.asInstanceOf[Uid].show + ":from")
      val idTo   = Uid(id.asInstanceOf[Uid].show + ":to")

      for {
        x1 <- rowIds.getMarker(idFrom).map(_.toRowIndex)
        y1 <- colIds.getMarker(idFrom).map(_.toColumnIndex)
        x2 <- rowIds.getMarker(idTo).map(_.toRowIndex)
        y2 <- colIds.getMarker(idTo).map(_.toColumnIndex)
        if x1 <= x2 && y1 <= y2
      } yield Range(SpreadsheetCoordinate(x1, y1), SpreadsheetCoordinate(x2, y2))

  def listRanges(): List[Range] = rangeIds.elements.toList.map(getRange).map(_.get)

  def listRangesWithIds: List[(RangeId, Range)] =
    rangeIds.elements.toList.flatMap { rid =>
      getRange(rid).map(rng => (rid, rng))
    }

  override def toString: String = pprint.apply(this).toString

  def printToConsole()(using LocalUid): Unit = {
    println("\nSpreadsheet Data Structure Print:")

    println(
      s"""|Replica Id: ${LocalUid.replicaId}
          |Size: ${rowIds.size}x${colIds.size}"""
        .stripMargin
    )

    val legend = "Row\\Col (Idx,Id)"

    val rowIdxAndId = rowIds.toList.zipWithIndex.map((dot, idx) => (idx, dot.time))
    val colIdxAndId = colIds.toList.zipWithIndex.map((dot, idx) => (idx, dot.time))

    val maxLen =
      rowAndColIdPairToContent.queryAllEntries.map { rs => rs.elements.mkString("/") }
        .concat(rowIdxAndId.map(_.toString))
        .concat(colIdxAndId.map(_.toString))
        .concat(Array(legend))
        .map(_.length)
        .maxOption.getOrElse(1)
        .max(1)

    val maxLenFmtStr = "%" + maxLen + "s"

    val sheetStr = rowIds.toList.zipWithIndex.map { (rowId, rowIdx) =>
      colIds.toList
        .map { colId =>
          val cellStr = rowAndColIdPairToContent
            .get((rowId, colId))
            .map(_.elements.mkString("/"))
            .filter(_.nonEmpty)
            .getOrElse("·")
          maxLenFmtStr.format(cellStr)
        }
        .mkString(
          s"${maxLenFmtStr.format(rowIdxAndId(rowIdx))} | ",
          " | ",
          " |"
        )
    }.mkString(" \n")

    println(
      s"""|${maxLenFmtStr.format(legend)}${colIdxAndId.map(maxLenFmtStr.format(_)).mkString(" | ", " | ", " |")}
          |$sheetStr\n"""
        .stripMargin
    )
  }
}

object Spreadsheet {
  type ElementId = Dot

  def empty[A]: Spreadsheet[A] = Spreadsheet[A]()

  case class SpreadsheetCoordinate(rowIdx: RowIndex, colIdx: ColumnIndex)

  case class Range(from: SpreadsheetCoordinate, to: SpreadsheetCoordinate) {
    def touchedRows(index: Int): Boolean = from.rowIdx == index || to.rowIdx == index
    def touchedCols(index: Int): Boolean = from.colIdx == index || to.colIdx == index

    def validAfterSwapping(source: Int, target: Int): Boolean =
        val plugInTarget =
          (coord: SpreadsheetCoordinate) =>
            source match {
              case coord.rowIdx => (target, coord.colIdx)
              case coord.colIdx => (coord.rowIdx, target)
              case _            => (coord.rowIdx, from.colIdx)
            }
        val newFrom = plugInTarget(from)
        val newTo   = plugInTarget(to)
        (newFrom._1 <= newTo._1) && (newFrom._2 <= newTo._2)
  }

  given bottom[A]: Bottom[Spreadsheet[A]]   = Bottom.provide(empty)
  given lattice[A]: Lattice[Spreadsheet[A]] = Lattice.derived
}
