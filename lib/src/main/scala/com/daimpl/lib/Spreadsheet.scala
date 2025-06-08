package com.daimpl.lib

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.alternatives.MultiValueRegister
import rdts.dotted.{HasDots, Obrem}
import rdts.time.{CausalTime, Dot, Dots, VectorClock}

case class Spreadsheet(
    colIds: ReplicatedList[Dot] = ReplicatedList.empty,
    rowIds: ReplicatedList[Dot] = ReplicatedList.empty,
    content: Map[Dot, Map[Dot, LastWriterWins[String | Null]]] = Map.empty,
    keepCols: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty,
    keepRows: ObserveRemoveMap[Dot, MultiValueRegister[Dot]] = ObserveRemoveMap.empty
) extends SpreadsheetLike[Obrem[Spreadsheet]] derives Lattice, HasDots, Bottom {
  type D = Obrem[Spreadsheet]

  def visibleRowIds: List[Dot] =
    rowIds.toList.filter(keepRows.queryKey(_).values.size > 0)

  def visibleColIds: List[Dot] =
    colIds.toList.filter(keepCols.queryKey(_).values.size > 0)

  def visibleRowIdxToRowIdx: List[Int] =
    rowIds.toList.zipWithIndex.filter(x => keepRows.queryKey(rowIds.read(x._2).get).values.size > 0).map(_._2)

  def visibleColIdxToColIdx: List[Int] =
    colIds.toList.zipWithIndex.filter(x => keepCols.queryKey(colIds.read(x._2).get).values.size > 0).map(_._2)

  def addRow()(using LocalUid)(using context: Dots): D = {
    val rowId = context.nextDot(LocalUid.replicaId)
    val operationId = context.nextDot(LocalUid.replicaId)
    val keepRowsUpdate = keepRows.update(rowId, keepRows.queryKey(rowId).write(LocalUid.replicaId, operationId))
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
    val keepColsUpdate = keepCols.update(colId, keepCols.queryKey(colId).write(LocalUid.replicaId, operationId))
    Obrem(
      Spreadsheet(
        colIds = colIds.append(colId).data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` Dots.single(colId),
      Dots.empty
    )
  }

  def removeRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val keepRowsUpdate = keepRows.update(rowIds.read(rowIdx).get, MultiValueRegister(Map.empty))
    Obrem(
      Spreadsheet(
        keepRows = keepRowsUpdate.data
      ),
      Dots.empty,
      rowIds.delete(rowIdx).context
    )
  }

  def removeColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val keepColsUpdate = keepCols.update(colIds.read(colIdx).get, MultiValueRegister(Map.empty))
    Obrem(
      Spreadsheet(
        keepCols = keepColsUpdate.data
      ),
      Dots.empty,
      colIds.delete(colIdx).context
    )
  }

  def insertRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = context.nextDot(LocalUid.replicaId)
    val operationId = context.nextDot(LocalUid.replicaId)
    val keepRowsUpdate = keepRows.update(rowId, keepRows.queryKey(rowId).write(LocalUid.replicaId, operationId))
    Obrem(
      Spreadsheet(
        rowIds = rowIds.insert(rowIdx, rowId).data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` Dots.single(rowId),
      Dots.empty
    )
  }

  def insertColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = context.nextDot(LocalUid.replicaId)
    val operationId = context.nextDot(LocalUid.replicaId)
    val keepColsUpdate = keepCols.update(colId, keepCols.queryKey(colId).write(LocalUid.replicaId, operationId))
    Obrem(
      Spreadsheet(
        colIds = colIds.insert(colIdx, colId).data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` Dots.single(colId),
      Dots.empty
    )
  }

  def editCell(visibleRowIdx: Int, visibleColIdx: Int, cellContent: String | Null)(using
      LocalUid
  )(using context: Dots): D = {
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = rowIds.read(rowIdx).get
    val rowOperationId = context.nextDot(LocalUid.replicaId)
    val keepRowsUpdate = keepRows.update(rowId, keepRows.queryKey(rowId).write(LocalUid.replicaId, rowOperationId))

    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = colIds.read(colIdx).get
    val colOperationId = context.nextDot(LocalUid.replicaId)
    val keepColsUpdate = keepCols.update(colId, keepCols.queryKey(colId).write(LocalUid.replicaId, colOperationId))

    val lastWriterWinsCellContent =
      if (content.get(colId).exists(_.contains(rowId)))
        content.get(colId).get(rowId).write(cellContent)
      else
        LastWriterWins(CausalTime.now(), cellContent)

    Obrem(
      Spreadsheet(
        rowIds = rowIds,
        colIds = colIds,
        content = Map(colId -> Map(rowId -> lastWriterWinsCellContent)),
        keepRows = keepRowsUpdate.data,
        keepCols = keepColsUpdate.data
      ),
      (keepRowsUpdate.observed `union` keepRowsUpdate.observed).add(colId).add(rowId),
      Dots.empty
    )
  }

  def printToConsole(): Unit = {
    val maxStringLength = content.values.flatMap(_.values).map(_.value.length()).maxOption().getOrElse(1)

    println(s"${visibleColIds.size}x${visibleRowIds.size}")
    for (r <- visibleRowIds) {
      val line = visibleColIds
        .map(c =>
          ("%" + maxStringLength + "s").format(
            content
              .get(c)
              .flatMap(_.get(r))
              .map(_.value)
              .getOrElse("·")
          )
        )
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

  def read(visibleColIdx: Int, visibleRowIdx: Int): Option[String] =
    val colIdx = visibleColIds(visibleColIdx)
    val rowIdx = visibleRowIds(visibleRowIdx)
    content.get(colIdx).flatMap(_.get(rowIdx)).map(_.payload)
}

object Spreadsheet {
  given HasDots[MultiValueRegister[Dot]] =
    HasDots.derived
}
