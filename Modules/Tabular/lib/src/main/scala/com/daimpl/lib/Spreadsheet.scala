package com.daimpl.lib

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import rdts.time.{Dot, Dots}

case class Spreadsheet[A](
    private val rowIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty,
    private val colIds: ReplicatedUniqueList[Dot] = ReplicatedUniqueList.empty,
    private val content: ObserveRemoveMap[(Dot, Dot), ReplicatedSet[A]] = ObserveRemoveMap.empty[(Dot, Dot), ReplicatedSet[A]]
) {

  lazy val observed: Dots =
    Dots.from(rowIds.toList)
    `union` Dots.from(colIds.toList)

  def addRow()(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.append(observed.nextDot))

  def addColumn()(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(colIds = colIds.append(observed.nextDot))
  }

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(rowIds = rowIds.removeIndex(rowIdx))
  }

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(colIds = colIds.removeIndex(colIdx))
  }

  def insertRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      rowIds = rowIds.insertAt(rowIdx, observed.nextDot),
    )
  }

  def insertColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      colIds = colIds.insertAt(colIdx, observed.nextDot),
    )
  }

  def editCell(rowIdx: Int, colIdx: Int, value: A)(using LocalUid): Spreadsheet[A] = {
    val rowId = rowIds.read(rowIdx).get
    val colId = colIds.read(colIdx).get
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
      rowIds = rowIds.update(rowIdx, rowId),
      colIds = colIds.update(colIdx, colId),
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

    val legend = "Row\\Col Id"

    val rowIdTimes = rowIds.toList.map(_.time)
    val colIdTimes = colIds.toList.map(_.time)

    val maxLen = content.queryAllEntries
      .map { rs => rs.elements.mkString("/") }
      .concat(rowIdTimes.map(_.toString))
      .concat(colIdTimes.map(_.toString))
      .concat(Array(legend))
      .map(_.length)
      .maxOption.getOrElse(1)
      .max(1)

    val maxLenFmtStr = "%" + maxLen + "s"

    val sheetStr = rowIds.toList.map { rowId =>
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
          s"${maxLenFmtStr.format(rowId.time)} | ", " | ", " |"
        )
    }.mkString(" \n")

    println(
      s"""|${maxLenFmtStr.format(legend)}${colIdTimes.map(maxLenFmtStr.format(_)).mkString(" | ", " | ", " |")}
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

  def getRow(visibleRowIdx: Int): List[ConflictableValue[A]] =
    (0 until numColumns).map(visibleColIdx => read(visibleColIdx, visibleRowIdx)).toList

  def toList: List[List[ConflictableValue[A]]] =
    (0 until numRows).map(getRow).toList

  def read(visibleColIdx: Int, visibleRowIdx: Int): ConflictableValue[A] =
    (for
      rowId <- rowIds.read(visibleRowIdx)
      colId <- colIds.read(visibleColIdx)
      cell  <- content.get((rowId, colId))
    yield ConflictableValue(cell.elements)).getOrElse(ConflictableValue.empty)

}

object Spreadsheet {
  given lattice[A]: Lattice[Spreadsheet[A]] = Lattice.derived
}
