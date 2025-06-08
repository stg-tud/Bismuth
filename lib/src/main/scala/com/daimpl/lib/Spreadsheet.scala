package com.daimpl.lib

import scala.collection.mutable.ArrayBuilder
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.alternatives.MultiValueRegister
import rdts.dotted.{Dotted, HasDots, Obrem}
import rdts.time.{CausalTime, Dot, Dots, Time, VectorClock}

import scala.collection.mutable

case class Spreadsheet(
  colIds:   ReplicatedList[Dot] = ReplicatedList.empty,
  rowIds:   ReplicatedList[Dot] = ReplicatedList.empty,
  content:  Map[Dot, Map[Dot, LastWriterWins[String | Null]]] = Map.empty,
  keepCols: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty,
  keepRows: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty
) extends SpreadsheetLike[Obrem[Spreadsheet]] derives Lattice, HasDots, Bottom
{
  type D = Obrem[Spreadsheet]

  def visibleRowIds: List[Dot] =
    rowIds.toList//.filter(keepRows.queryKey(_).values.size > 0)

  def visibleColIds: List[Dot] =
    colIds.toList//.filter(keepCols.queryKey(_).values.size > 0)

  def addRow()(using LocalUid)(using context: Dots): D = {
    val rowId = context.nextDot(LocalUid.replicaId)
    val operationId = context.nextDot(LocalUid.replicaId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(LocalUid.replicaId -> Time.current())) -> operationId)))
    Obrem(
      Spreadsheet(
        rowIds = rowIds.append(rowId).data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` Dots.single(rowId),
      Dots.empty
    )
  }

  def addColumn()(using LocalUid)(using context: Dots): D = {
    val colId = context.nextDot(LocalUid.replicaId)
    val operationId = context.nextDot(LocalUid.replicaId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(LocalUid.replicaId -> Time.current())) -> operationId)))
    Obrem(
      Spreadsheet(
        colIds = colIds.append(colId).data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` Dots.single(colId),
      Dots.empty
    )
  }

  def removeRow(rowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val keepRowsUpdate = keepRows.update(rowIds.read(rowIdx).get, MultiValueRegister(Map.empty))
    Obrem(
      Spreadsheet(
        keepRows = keepRowsUpdate.data
      ),
      Dots.empty,
      rowIds.delete(rowIdx).context,
    )
  }

  def removeColumn(colIdx: Int)(using LocalUid)(using context: Dots): D = {
    val keepColsUpdate = keepCols.update(colIds.read(colIdx).get, MultiValueRegister(Map.empty))
    Obrem(
      Spreadsheet(
        keepCols = keepColsUpdate.data
      ),
      Dots.empty,
      colIds.delete(colIdx).context
    )
  }

  def insertRow(rowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val rowId = context.nextDot(LocalUid.replicaId)
    Obrem(
      Spreadsheet(
        rowIds = rowIds.insert(rowIdx, rowId).data
      ),
      Dots.from(keepRows.queryKey(rowId).values) `union` Dots.single(rowId),
      Dots.empty
    )
  }

  def insertColumn(colIdx: Int)(using LocalUid)(using context: Dots): D = {
    val colId = context.nextDot(LocalUid.replicaId)
    Obrem(
      Spreadsheet(
        colIds = colIds.insert(colIdx, colId).data
      ),
      Dots.from(keepCols.queryKey(colId).values) `union` Dots.single(colId),
      Dots.empty
    )
  }

  def editCell(rowIdx: Int, colIdx: Int, content: String | Null)(using LocalUid)(using context: Dots): D =
  {
    val rowId = rowIds.read(rowIdx).get
    val colId = colIds.read(colIdx).get

    val operation1Id = context.nextDot(LocalUid.replicaId)
    val operation2Id = context.nextDot(LocalUid.replicaId)

    val keepRowsUpdate = keepRows.update(rowId, keepRows.queryKey(rowId).write(LocalUid.replicaId, operation1Id))
    val keepColsUpdate = keepCols.update(colId, keepCols.queryKey(colId).write(LocalUid.replicaId, operation2Id))

    Obrem(
      Spreadsheet(
        rowIds = rowIds, colIds = colIds,
        content = Map(colId -> Map(rowId -> LastWriterWins(CausalTime.now(), content))),
        keepRows = keepRowsUpdate.data,
        keepCols = keepColsUpdate.data
      ),
      (keepRowsUpdate.observed `union` keepRowsUpdate.observed).add(colId).add(rowId),
      Dots.empty
    )
  }

  def printToConsole(): Unit = {
    val maxStringLength = content.values.flatMap(_.values).map(_.value.length()).maxOption().getOrElse(1)

    println(s"${colIds.toList.size}x${rowIds.toList.size}")
    for (r <- rowIds.toList) {
      val line = colIds.toList
        .map(c => ("%" + maxStringLength + "s").format(content.get(c)
          .flatMap(_.get(r))
          .map(_.value)
          .getOrElse("·")
        ))
        .mkString("| ", " | ", " |")
      println(line)
    }
  }

  def purgeTombstones()(using LocalUid, Dots): D = {
    val colDelta = colIds.purgeTombstones()
    val rowDelta = rowIds.purgeTombstones()

    Obrem(
      Spreadsheet(colDelta.data, rowDelta.data),
      colDelta.context `union` rowDelta.context,
      Dots.empty
    )
  }

  def numRows: Int = visibleRowIds.size

  def numColumns: Int = visibleColIds.size

  def getRow(rowIdx: Int): List[Option[String]] =
    (0 until numColumns).map(colIdx => read(colIdx, rowIdx)).toList

  def toList: List[List[Option[String]]] =
    (0 until numRows).map(getRow).toList

  def read(colIdx: Int, rowIdx: Int): Option[String] =
    val col = visibleColIds(colIdx)
    val row = visibleRowIds(rowIdx)
    content.get(col).flatMap(_.get(row)).map(_.payload)
}

object Spreadsheet {
  given HasDots[MultiValueRegister[Dot]] =
    HasDots.derived
}
