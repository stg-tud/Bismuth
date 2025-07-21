package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.{Range, SpreadsheetCoordinate, empty}
import rdts.base.{Bottom, Decompose, Lattice, LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import rdts.time.{Dot, Dots}

case class Spreadsheet[A](
   private val rowIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty,
   private val colIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty,
   private val content: ObserveRemoveMap[(Dot, Dot), ReplicatedSet[A]] = ObserveRemoveMap.empty[(Dot, Dot), ReplicatedSet[A]],
   private val ranges: ReplicatedSet[Uid] = ReplicatedSet.empty
){

  lazy val observed: Dots =
    Dots.from(rowIds.toList)
      `union` Dots.from(colIds.toList)

  def addRow()(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.append(observed.nextDot))

  def addColumn()(using LocalUid): Spreadsheet[A] =
    Spreadsheet(colIds = colIds.append(observed.nextDot))

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.removeAt(rowIdx))

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(colIds = colIds.removeAt(colIdx))

  def insertRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.insertAt(rowIdx, observed.nextDot))

  def insertColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(colIds = colIds.insertAt(colIdx, observed.nextDot))

  def moveRow(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.move(sourceIdx, targetIdx))

  def moveColumn(sourceIdx: Int, targetIdx: Int)(using LocalUid): Spreadsheet[A] =
    Spreadsheet(colIds = colIds.move(sourceIdx, targetIdx))

  def editCell(coordinate: SpreadsheetCoordinate, value: A)(using LocalUid): Spreadsheet[A] = {
    val rowId = rowIds.read(coordinate.rowIdx).get
    val colId = colIds.read(coordinate.colIdx).get
    val newContent =
      if value == null then
        content.transform((rowId, colId)) {
          case None => None
          case Some(set) => Some(set.clear())
        }
      else
        content.transform((rowId, colId)) {
          case None => Some(ReplicatedSet.empty.add(value))
          case Some(set) => Some(Lattice.merge(set.removeBy(_ != value), set.add(value)))
        }
    Spreadsheet(
      rowIds = rowIds.update(coordinate.rowIdx, rowId),
      colIds = colIds.update(coordinate.colIdx, colId),
      content = newContent
    )
  }

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
      content.queryAllEntries.map{ rs => rs.elements.mkString("/") }
      .concat(rowIdxAndId.map(_.toString))
      .concat(colIdxAndId.map(_.toString))
      .concat(Array(legend))
      .map(_.length)
      .maxOption.getOrElse(1)
      .max(1)

    val maxLenFmtStr = "%" + maxLen + "s"

    val sheetStr = rowIds.toList.zipWithIndex.map{ (rowId, rowIdx) =>
      colIds.toList
        .map { colId =>
          val cellStr = content
            .get((rowId, colId))
            .map(_.elements.mkString("/"))
            .filter(_.nonEmpty)
            .getOrElse("·")
          maxLenFmtStr.format(cellStr)
        }
        .mkString(
          s"${maxLenFmtStr.format(rowIdxAndId(rowIdx))} | ", " | ", " |"
        )
    }.mkString(" \n")

    println(
      s"""|${maxLenFmtStr.format(legend)}${colIdxAndId.map(maxLenFmtStr.format(_)).mkString(" | ", " | ", " |")}
          |$sheetStr\n"""
      .stripMargin
    )
  }

  def purgeTombstones()(using LocalUid): Spreadsheet[A] = {
    val rows = rowIds.toList
    val cols = colIds.toList
    Spreadsheet(
      content = content.removeBy((row, col) => !rows.contains(row) || !cols.contains(col))
    )
  }

  def numRows: Int = rowIds.size

  def numColumns: Int = colIds.size

  def getRow(rowIdx: Int): List[ConflictableValue[A]] =
    (0 until numColumns).map(colIdx => read(SpreadsheetCoordinate(rowIdx, colIdx))).toList

  def toList: List[List[ConflictableValue[A]]] =
    (0 until numRows).map(getRow).toList

  def read(coordinate: SpreadsheetCoordinate): ConflictableValue[A] =
    (for
      rowId <- rowIds.read(coordinate.rowIdx)
      colId <- colIds.read(coordinate.colIdx)
      cell  <- content.get((rowId, colId))
    yield ConflictableValue(cell.elements)).getOrElse(ConflictableValue.empty)

  def addRange(id: Uid, from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)(using LocalUid): Spreadsheet[A] =
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")
    Spreadsheet(
      rowIds  = rowIds.addMarker(idFrom, from.rowIdx)
        `merge` rowIds.addMarker(idTo  , to.rowIdx),
      colIds  = colIds.addMarker(idFrom, from.colIdx)
        `merge` colIds.addMarker(idTo  , to.colIdx),
      ranges = ranges.add(id)
    )

  def removeRange(id: Uid): Spreadsheet[A] =
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")
    Spreadsheet(
      rowIds  = rowIds.removeMarker(idFrom)
        `merge` rowIds.removeMarker(idTo),
      colIds  = colIds.removeMarker(idFrom)
        `merge` colIds.removeMarker(idTo),
      ranges = ranges.remove(id)
    )

  def getRange(id: Uid): Option[Range] =
    val idFrom = Uid(id.show + ":from")
    val idTo   = Uid(id.show + ":to")

    for {
      x1 <- rowIds.getMarker(idFrom)
      y1 <- colIds.getMarker(idFrom)
      x2 <- rowIds.getMarker(idTo)
      y2 <- colIds.getMarker(idTo)
    } yield Range(SpreadsheetCoordinate(x1,y1), SpreadsheetCoordinate(x2,y2))

  def listRanges(): List[Range] = ranges.elements.toList.map(getRange).map(_.get)

  def listRangesWithIds: List[(Uid, Range)] =
    ranges.elements.toList.flatMap { rid =>
      getRange(rid).map(rng => (rid, rng))
    }
}

object Spreadsheet {

  def empty[A]: Spreadsheet[A] = Spreadsheet[A]()

  case class SpreadsheetCoordinate(rowIdx: Int, colIdx: Int)

  case class Range(from: SpreadsheetCoordinate, to: SpreadsheetCoordinate)

  given bottom[A]: Bottom[Spreadsheet[A]] = Bottom.provide(empty)
  given lattice[A]: Lattice[Spreadsheet[A]] = Lattice.derived
  given decompose[A]: Decompose[Spreadsheet[A]] = Decompose.derived
}
