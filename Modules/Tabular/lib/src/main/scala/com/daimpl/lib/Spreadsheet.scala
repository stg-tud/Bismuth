package com.daimpl.lib

import rdts.base.{DecoratedLattice, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedList}
import rdts.time.{Dot, Dots}

import scala.util.chaining.scalaUtilChainingOps

case class Spreadsheet[A](
    rowIds: ReplicatedList[Dot] = ReplicatedList.empty,
    colIds: ReplicatedList[Dot] = ReplicatedList.empty,
    content: ObserveRemoveMap[(Dot, Dot), LastWriterWins[A]] = ObserveRemoveMap.empty[(Dot, Dot), LastWriterWins[A]],
) {

  lazy val observed: Dots =
    Dots.from(rowIds.elements.values.map(_.read)) `union`
    Dots.from(colIds.elements.values.map(_.read))

  def addRow()(using LocalUid): Spreadsheet[A] =
    Spreadsheet(rowIds = rowIds.append(observed.nextDot))

  def addColumn()(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(colIds = colIds.append(observed.nextDot))
  }

  def removeRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(rowIds = rowIds.delete(rowIdx))
  }

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(colIds = colIds.delete(colIdx))
  }

  def insertRow(rowIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      rowIds = rowIds.insert(rowIdx, observed.nextDot),
    )
  }

  def insertColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      colIds = colIds.insert(colIdx, observed.nextDot),
    )
  }

  def editCell(rowIdx: Int, colIdx: Int, cellContent: A)(using LocalUid): Spreadsheet[A] = {
    val rowId = rowIds.read(rowIdx).get
    val colId = colIds.read(colIdx).get
    Spreadsheet(content = content.transform((rowId, colId)) {
      case None      => Some(LastWriterWins.now(cellContent))
      case Some(lww) => Some(lww.write(cellContent))
    })

  }

  def printToConsole(): Unit = {
    println(rowIds.toList)
    println(colIds.toList)
    println(content.queryAllEntries.map(_.value).mkString(", "))

    val maxStringLength =
      content.queryAllEntries.map(_.value).filterNot(_ == null).map(_.toString.length()).maxOption.getOrElse(1)

    println(s"${colIds.size}x${rowIds.size}")
    val res = rowIds.toList.map(rowId => {
      val line = colIds.toList
        .map(colId =>
          ("%" + maxStringLength + "s").format(
            content
              .get((colId, rowId))
              .map(_.payload)
              .getOrElse("·")
          )
        ).mkString(s"${rowId} | ", " | ", " |")
      line
    }).mkString(" \n")
    println(s"${colIds.toList.mkString(s"| ", " | ", " |")}\n$res")
  }

  def purgeTombstones()(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      rowIds = rowIds.purgeTombstones(),
      colIds = colIds.purgeTombstones(),
    )
  }

  def numRows: Int = rowIds.size

  def numColumns: Int = colIds.size

  def getRow(visibleRowIdx: Int): List[Option[A]] =
    (0 until numColumns).map(visibleColIdx => read(visibleColIdx, visibleRowIdx)).toList

  def toList: List[List[Option[A]]] =
    (0 until numRows).map(getRow).toList

  def read(visibleColIdx: Int, visibleRowIdx: Int): Option[A] =
    for
      rowId <- rowIds.read(visibleRowIdx)
      colId <- colIds.read(visibleColIdx)
      elem  <- content.get((rowId, colId))
    yield elem.payload
}

object Spreadsheet {

  given lattice[A]: Lattice[Spreadsheet[A]] =
    DecoratedLattice.compact[Spreadsheet[A]](Lattice.derived[Spreadsheet[A]]) { merged =>
      val rows = Dots.from(merged.rowIds.toList)
      val cols = Dots.from(merged.colIds.toList)
      merged.copy(content = merged.content.removeBy((row, col) => !(rows.contains(row) && cols.contains(col))))
    }
}
