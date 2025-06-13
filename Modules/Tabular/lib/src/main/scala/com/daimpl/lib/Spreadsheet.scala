package com.daimpl.lib

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.alternatives.MultiValueRegister
import rdts.dotted.{HasDots, Obrem}
import rdts.time.{CausalTime, Dot, Dots, VectorClock}

case class Spreadsheet(
    rowIds: ReplicatedList[Dot] = ReplicatedList.empty,
    colIds: ReplicatedList[Dot] = ReplicatedList.empty,
    content: ObserveRemoveMap[(Dot, Dot), LastWriterWins[String | Null]] = ObserveRemoveMap.empty,
    keepRows: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty,
    keepCols: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty
) extends SpreadsheetLike[Obrem[Spreadsheet]] derives Lattice, HasDots, Bottom {
  type D = Obrem[Spreadsheet]

  given HasDots[MultiValueRegister[Dot]] =
    HasDots.derived

  given Bottom[String] = Bottom.provide("")

  def visibleRowIds: List[Dot] =
    rowIds.toList.filter(keepRows.get(_).exists(_.values.nonEmpty))

  def visibleColIds: List[Dot] =
    colIds.toList.filter(keepCols.get(_).exists(_.values.nonEmpty))

  private def visibleRowIdxToRowIdx: List[Int] =
    rowIds.toList.zipWithIndex.filter(p => keepRows.get(p._1).exists(_.values.nonEmpty)).map(_._2)

  private def visibleColIdxToColIdx: List[Int] =
    colIds.toList.zipWithIndex.filter(p => keepCols.get(p._1).exists(_.values.nonEmpty)).map(_._2)

  def addRow()(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val rowId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val rowIdsUpdate = rowIds.append(rowId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        rowIds = rowIdsUpdate.data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` rowIdsUpdate.context `union` Dots.single(rowId) `union` Dots.single(operationId),
      Dots.empty
    )
  }

  def addColumn()(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val colId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val colIdsUpdate = colIds.append(colId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        colIds = colIdsUpdate.data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` colIdsUpdate.context `union` Dots.single(colId) `union` Dots.single(operationId),
      Dots.empty
    )
  }

  def removeRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = rowIds.read(rowIdx).get
    val keepRowsDeleted = keepRows.remove(rowId)
    Obrem(
      Spreadsheet(
        keepRows = keepRowsDeleted.data
      ),
      Dots.empty,
      keepRowsDeleted.deletions //`union` rowIds.delete(rowIdx).context
    )
  }

  def removeColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = colIds.read(colIdx).get
    val keepColsDeleted = keepCols.remove(colId)
    Obrem(
      Spreadsheet(
        keepCols = keepColsDeleted.data
      ),
      Dots.empty,
      keepColsDeleted.deletions //`union` colIds.delete(colIdx).context
    )
  }

  def insertRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val rowIdsUpdate = rowIds.insert(rowIdx, rowId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        rowIds = rowIdsUpdate.data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` rowIdsUpdate.context `union` Dots.single(rowId) `union` Dots.single(operationId),
      Dots.empty
    )
  }

  def insertColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val colIdsUpdate = colIds.insert(colIdx, colId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        colIds = colIdsUpdate.data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` colIdsUpdate.context `union` Dots.single(colId) `union` Dots.single(operationId),
      Dots.empty
    )
  }

  def editCell(visibleRowIdx: Int, visibleColIdx: Int, cellContent: String | Null)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId

    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val colIdx = visibleColIdxToColIdx(visibleColIdx)

    val rowId = rowIds.read(rowIdx).get
    val keepRowsDeleted = keepRows.remove(rowId)
    val rowOperationId = context.nextDot(replicaId)
    val keepRowsUpdate = keepRowsDeleted.data.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> rowOperationId)))

    val colId = colIds.read(colIdx).get
    val keepColsDeleted = keepCols.remove(colId)
    val colOperationId = context.nextDot(replicaId)
    val keepColsUpdate = keepColsDeleted.data.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> colOperationId)))

    val lastWriterWinsCellContent =
      if (content.contains((colId, rowId)))
        content.queryKey((colId, rowId)).write(cellContent)
      else
        LastWriterWins.now(cellContent)

    val contentUpdate = content.update((colId, rowId), lastWriterWinsCellContent)

    Obrem(
      Spreadsheet(
        content = contentUpdate.data,
        keepRows = keepRowsDeleted.data `merge` keepRowsUpdate.data,
        keepCols = keepColsDeleted.data `merge` keepColsUpdate.data
      ),
      contentUpdate.observed `union` keepRowsUpdate.observed `union` keepColsUpdate.observed `union` Dots.from(Array(rowId, colId, rowOperationId, colOperationId)),
      keepRowsDeleted.deletions `union` keepColsDeleted.deletions
    )
  }

  def printToConsole(): Unit = {
    println(rowIds)
    println(colIds)
    println(keepRows)
    println(keepCols)

    val maxStringLength = content.queryAllEntries.map(_.value).filterNot(_ == null).map(_.length()).maxOption().getOrElse(1)

    println(s"${visibleColIds.size}x${visibleRowIds.size}")
    visibleRowIds.foreach(
      rowId => {
        val line = visibleColIds
          .map(colId =>
            ("%" + maxStringLength + "s").format(
              content
                .get((colId, rowId))
                .map(_.payload)
                .getOrElse("·")
            )
          ).mkString("| ", " | ", " |")
        println(line)
      }
    )
  }

  def purgeTombstones()(using LocalUid, Dots): D = {
    val rowDelta = rowIds.purgeTombstones()
    val colDelta = colIds.purgeTombstones()

    Obrem(
      Spreadsheet(
        rowIds = rowDelta.data,
        colIds = colDelta.data
      ),
      rowDelta.context `union` colDelta.context,
      Dots.empty
    )
  }

  def numRows: Int = visibleRowIds.size

  def numColumns: Int = visibleColIds.size

  def getRow(visibleRowIdx: Int): List[Option[String]] =
    (0 until numColumns).map(visibleColIdx => read(visibleColIdx, visibleRowIdx)).toList

  def toList: List[List[Option[String]]] =
    (0 until numRows).map(getRow).toList

  def read(visibleColIdx: Int, visibleRowIdx: Int): Option[String] =
    val rowId = visibleRowIds(visibleRowIdx)
    val colId = visibleColIds(visibleColIdx)
    content.get((colId, rowId)).map(_.payload)
}

object Spreadsheet {
  given HasDots[MultiValueRegister[Dot]] =
    HasDots.derived

  given Bottom[LastWriterWins[String | Null]] =
    given Bottom[String] = Bottom.provide("")
    Bottom.derived
}
