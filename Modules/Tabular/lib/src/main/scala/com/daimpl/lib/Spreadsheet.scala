package com.daimpl.lib

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedList, ReplicatedSet}
import rdts.time.{Dot, Dots}

case class Spreadsheet[A](
    rowIds: ReplicatedList[Dot] = ReplicatedList.empty,
    colIds: ReplicatedList[Dot] = ReplicatedList.empty,
    content: ObserveRemoveMap[(Dot, Dot), ReplicatedSet[A]] = ObserveRemoveMap.empty[(Dot, Dot), ReplicatedSet[A]]
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
    val removed = rowIds.read(rowIdx)
    Spreadsheet(rowIds = rowIds.delete(rowIdx), content = content.removeBy((row, col) => removed.contains(row)))
  }

  def removeColumn(colIdx: Int)(using LocalUid): Spreadsheet[A] = {
    val removed = colIds.read(colIdx)
    Spreadsheet(colIds = colIds.delete(colIdx), content = content.removeBy((row, col) => removed.contains(col)))
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

  def editCell(rowIdx: Int, colIdx: Int, value: A)(using LocalUid): Spreadsheet[A] = {
    val rowId = rowIds.read(rowIdx).get
    val colId = colIds.read(colIdx).get
    if (value == null) {
      return Spreadsheet(content = content.transform((rowId, colId)) {
        case None => None
        case Some(set) => Some(set.clear())
      })
    }
    Spreadsheet(content = content.transform((rowId, colId)) {
      case None      => Some(ReplicatedSet.empty.add(value))
      case Some(set) => Some(Lattice.merge(set.removeBy(_ != value), set.add(value)))
    })
  }

  def printToConsole(): Unit = {
    println(rowIds.toList)
    println(colIds.toList)

    val maxLen = content.queryAllEntries
      .map { rs => rs.elements.mkString("/")}
      .map(_.length)
      .maxOption.getOrElse(1)
      .max(1)

    println(s"${colIds.size}x${rowIds.size}")

    val sheetStr = rowIds.toList.map { rowId =>
      colIds.toList
        .map { colId =>
          val cellStr = content
            .get((rowId, colId))
            .map(_.elements.mkString("/"))
            .filter(_.nonEmpty)
            .getOrElse("·")
          ("%" + maxLen + "s").format(cellStr)
        }
        .mkString(s"${rowId} | ", " | ", " |")
    }.mkString(" \n")

    println(s"${colIds.toList.mkString("| ", " | ", " |")}\n$sheetStr")
  }

  def purgeTombstones()(using LocalUid): Spreadsheet[A] = {
    Spreadsheet(
      rowIds = rowIds.purgeTombstones(),
      colIds = colIds.purgeTombstones(),
    )
  }

  def numRows: Int = rowIds.size

  def numColumns: Int = colIds.size

  def getRow(visibleRowIdx: Int): List[Set[A]] =
    (0 until numColumns).map(visibleColIdx => read(visibleColIdx, visibleRowIdx)).toList

  def toList: List[List[Set[A]]] =
    (0 until numRows).map(getRow).toList

  def read(visibleColIdx: Int, visibleRowIdx: Int): Set[A] =
    (for
      rowId <- rowIds.read(visibleRowIdx)
      colId <- colIds.read(visibleColIdx)
      cell  <- content.get((rowId, colId))
    yield cell.elements).getOrElse(Set.empty)

}

object Spreadsheet {

  given lattice[A]: Lattice[Spreadsheet[A]] = Lattice.derived
}
