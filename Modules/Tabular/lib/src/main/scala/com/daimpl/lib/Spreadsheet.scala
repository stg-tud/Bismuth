package com.daimpl.lib

import rdts.base.{Bottom, DecoratedLattice, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedList}
import rdts.time.{CausalTime, Dot, Dots}

case class Spreadsheet(
    rowIds: ReplicatedList[Dot] = ReplicatedList.empty,
    colIds: ReplicatedList[Dot] = ReplicatedList.empty,
    content: ObserveRemoveMap[(Dot, Dot), LastWriterWins[String | Null]] = ObserveRemoveMap.empty,
) {

  lazy val observed: Dots = rowIds.observed `union` colIds.observed

  def addRow()(using LocalUid): Spreadsheet =
    Spreadsheet(rowIds = rowIds.append(observed.nextDot))

  def addColumn()(using LocalUid): Spreadsheet = {
    Spreadsheet(colIds = colIds.append(observed.nextDot))
  }

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet = {
    Spreadsheet(rowIds = rowIds.delete(rowIdx))
  }

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet = {
    Spreadsheet(colIds = colIds.delete(colIdx))
  }

  def insertRow(rowIdx: Int)(using LocalUid): Spreadsheet = {
    Spreadsheet(
      rowIds = rowIds.insert(rowIdx, observed.nextDot),
    )
  }

  def insertColumn(colIdx: Int)(using LocalUid): Spreadsheet = {
    Spreadsheet(
      colIds = colIds.insert(colIdx, observed.nextDot),
    )
  }

  def editCell(rowIdx: Int, colIdx: Int, cellContent: String | Null)(using LocalUid): Spreadsheet = {
    val rowId = rowIds.read(rowIdx).get
    val colId = colIds.read(colIdx).get
    Spreadsheet(content = content.transform((rowId, colId)) {
      case None      => Some(LastWriterWins.now(cellContent))
      case Some(lww) => Some(lww.write(cellContent))
    })

  }

  def printToConsole(): Unit = {
    println(rowIds)
    println(colIds)

    val maxStringLength =
      content.queryAllEntries.map(_.value).filterNot(_ == null).map(_.length()).maxOption.getOrElse(1)

    println(s"${colIds.size}x${rowIds.size}")
    rowIds.toList.foreach(rowId => {
      val line = colIds.toList
        .map(colId =>
          ("%" + maxStringLength + "s").format(
            content
              .get((colId, rowId))
              .map(_.payload)
              .getOrElse("·")
          )
        ).mkString("| ", " | ", " |")
      println(line)
    })
  }

  def purgeTombstones()(using LocalUid): Spreadsheet = {
    Spreadsheet(
      rowIds = rowIds.purgeTombstones(),
      colIds = colIds.purgeTombstones(),
    )
  }

  def numRows: Int = rowIds.size

  def numColumns: Int = colIds.size

  def getRow(visibleRowIdx: Int): List[Option[String]] =
    (0 until numColumns).map(visibleColIdx => read(visibleColIdx, visibleRowIdx)).toList

  def toList: List[List[Option[String]]] =
    (0 until numRows).map(getRow).toList

  def read(visibleColIdx: Int, visibleRowIdx: Int): Option[String] =
    for
      rowId <- rowIds.read(visibleRowIdx)
      colId <- colIds.read(visibleColIdx)
      elem  <- content.get((colId, rowId))
    yield elem.payload
}

object Spreadsheet {

  given Bottom[LastWriterWins[String | Null]] =
    given Bottom[String] = Bottom.provide("")
    Bottom.derived

  given lattice[A]: Lattice[Spreadsheet] = DecoratedLattice.compact[Spreadsheet](Lattice.derived) { merged =>
    val rows = Dots.from(merged.rowIds.toList)
    val cols = Dots.from(merged.colIds.toList)
    merged.copy(content = merged.content.removeBy((row, col) => rows.contains(row) && cols.contains(col)) )
  }
}
