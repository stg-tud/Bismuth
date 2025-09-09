package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.MarkerRemovalBehavior
import com.daimpl.lib.Spreadsheet.{Range, RangeId, SpreadsheetCoordinate, empty}
import rdts.base.{Bottom, Decompose, Lattice, LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import rdts.time.{Dot, Dots}

case class Spreadsheet[A](
    private val rowIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty[Dot],
    private val colIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty[Dot],
    private val rowAndColIdPairToContent: ObserveRemoveMap[(rowId: Dot, colId: Dot), ReplicatedSet[A]] =
      ObserveRemoveMap.empty[(rowId: Dot, colId: Dot), ReplicatedSet[A]],
    private val rangeIds: ReplicatedSet[RangeId] = ReplicatedSet.empty[RangeId]
) {
  lazy val observed: Dots =
    Dots.from(rowIds.toList)
    `union` Dots.from(colIds.toList)

  private def newRowOrColId(using LocalUid): Dot = observed.nextDot(LocalUid.replicaId)

  def addRow()(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](rowIds = rowIds.append(newRowOrColId))

  def addColumn()(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](colIds = colIds.append(newRowOrColId))

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](rowIds = rowIds.removeAt(rowIdx))

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](colIds = colIds.removeAt(colIdx))

  def insertRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](rowIds = rowIds.insertAt(rowIdx, newRowOrColId))

  def insertColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet[A](colIds = colIds.insertAt(colIdx, newRowOrColId))

  def moveRow(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A] =
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

  def moveColumn(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A] =
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

  def editCell(coordinate: SpreadsheetCoordinate, value: A)(using LocalUid): Spreadsheet[A] = {
    val rowId      = rowIds.readAt(coordinate.rowIdx).get
    val colId      = colIds.readAt(coordinate.colIdx).get
    val newContent =
      rowAndColIdPairToContent.transform(rowId, colId) {
        if value == null then
          case None      => None
          case Some(set) => Some(set.clear())
        else
          case None      => Some(ReplicatedSet.empty.add(value))
          case Some(set) => Some(set.removeBy(_ != value) `merge` set.add(value))
      }
    Spreadsheet[A](
      rowIds = rowIds.updateAt(coordinate.rowIdx, rowId),
      colIds = colIds.updateAt(coordinate.colIdx, colId),
      rowAndColIdPairToContent = newContent
    )
  }

  def purgeTombstones(using LocalUid): Spreadsheet[A] = {
    val rows = rowIds.toList
    val cols = colIds.toList
    Spreadsheet[A](rowAndColIdPairToContent = rowAndColIdPairToContent.removeBy {
      idCoord => !(rows.contains(idCoord.rowId) && cols.contains(idCoord.colId))
    })
  }

  def numRows: Int    = rowIds.size
  def numColumns: Int = colIds.size

  private def getRow(rowIdx: Int): List[ConflictableValue[A]] =
    (0 until numColumns).map(colIdx => read(SpreadsheetCoordinate(rowIdx, colIdx))).toList

  def toList: List[List[ConflictableValue[A]]] =
    (0 until numRows).map(getRow).toList

  def read(coordinate: SpreadsheetCoordinate): ConflictableValue[A] =
    (for
      rowId <- rowIds.readAt(coordinate.rowIdx)
      colId <- colIds.readAt(coordinate.colIdx)
      cell  <- rowAndColIdPairToContent.get(rowId, colId)
    yield ConflictableValue(cell.elements)).getOrElse(ConflictableValue.empty[A])

  def addRange(id: RangeId, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[A] =
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")
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
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")
    Spreadsheet[A](
      rowIds = rowIds.removeMarker(idFrom)
        `merge` rowIds.removeMarker(idTo),
      colIds = colIds.removeMarker(idFrom)
        `merge` colIds.removeMarker(idTo),
      rangeIds = rangeIds.remove(id)
    )

  def getRange(id: RangeId): Option[Range] =
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")

    for {
      x1 <- rowIds.getMarker(idFrom)
      y1 <- colIds.getMarker(idFrom)
      x2 <- rowIds.getMarker(idTo)
      y2 <- colIds.getMarker(idTo)
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
  type RangeId   = Uid

  def empty[A]: Spreadsheet[A] = Spreadsheet[A]()

  case class SpreadsheetCoordinate(rowIdx: Int, colIdx: Int)

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
