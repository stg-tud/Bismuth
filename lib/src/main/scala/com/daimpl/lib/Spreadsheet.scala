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
    rowIds.toList.filter(keepRows.queryKey(_).values.size > 0)

  def visibleColIds: List[Dot] =
    colIds.toList.filter(keepCols.queryKey(_).values.size > 0)

  def visibleRowIdxToRowIdx: List[Int] =
    rowIds.toList.zipWithIndex.filter(x => keepRows.queryKey(rowIds.read(x._2).get).values.size > 0).map(_._2)

  def visibleColIdxToColIdx: List[Int] =
    colIds.toList.zipWithIndex.filter(x => keepCols.queryKey(colIds.read(x._2).get).values.size > 0).map(_._2)

  def addRow()(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val rowId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        rowIds = rowIds.append(rowId).data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` Dots.single(rowId), //`union` Dots.single(operationId),
      Dots.empty
    )
  }

  def addColumn()(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val colId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        colIds = colIds.append(colId).data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` Dots.single(colId), //`union` Dots.single(operationId),
      Dots.empty
    )
  }

  def removeRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = rowIds.read(rowIdx).get
    //val oldKeepRows = keepRows.queryKey(rowId)
    val keepRowsUpdate = keepRows.remove(rowId)
    Obrem(
      Spreadsheet(
        keepRows = keepRowsUpdate.data
      ),
      Dots.empty,
      /*Dots.from(oldKeepRows.values) `union`*/ keepRowsUpdate.deletions //`union` rowIds.delete(rowIdx).context
    )
  }

  def removeColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = colIds.read(colIdx).get
    //val oldKeepCols = keepCols.queryKey(colId)
    val keepColsUpdate = keepCols.remove(colId)
    Obrem(
      Spreadsheet(
        keepCols = keepColsUpdate.data
      ),
      Dots.empty,
      /*Dots.from(oldKeepCols.values) `union`*/ keepColsUpdate.deletions //`union` colIds.delete(colIdx).context
    )
  }

  def insertRow(visibleRowIdx: Int)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        rowIds = rowIds.insert(rowIdx, rowId).data,
        keepRows = keepRowsUpdate.data
      ),
      keepRowsUpdate.observed `union` Dots.single(rowId), //`union` Dots.single(operationId),
      Dots.empty
    )
  }

  def insertColumn(visibleColIdx: Int)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId
    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = context.nextDot(replicaId)
    val operationId = context.nextDot(replicaId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> operationId)))
    Obrem(
      Spreadsheet(
        colIds = colIds.insert(colIdx, colId).data,
        keepCols = keepColsUpdate.data
      ),
      keepColsUpdate.observed `union` Dots.single(colId), //`union` Dots.single(operationId),
      Dots.empty
    )
  }

  def editCell(visibleRowIdx: Int, visibleColIdx: Int, cellContent: String | Null)(using LocalUid)(using context: Dots): D = {
    val replicaId = LocalUid.replicaId

    val rowIdx = visibleRowIdxToRowIdx(visibleRowIdx)
    val rowId = rowIds.read(rowIdx).get
    val rowOperationId = context.nextDot(replicaId)
    //val oldKeepRows = keepRows.queryKey(rowId)
    val keepRowsUpdate = keepRows.update(rowId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> rowOperationId)))

    val colIdx = visibleColIdxToColIdx(visibleColIdx)
    val colId = colIds.read(colIdx).get
    val colOperationId = context.nextDot(replicaId)
    //val oldKeepCols = keepCols.queryKey(colId)
    val keepColsUpdate = keepCols.update(colId, MultiValueRegister(Map(VectorClock(Map(replicaId -> CausalTime.now().time)) -> colOperationId)))

    val lastWriterWinsCellContent =
      if (content.contains((colId, rowId)))
        content.queryKey((colId, rowId)).write(cellContent)
      else
        LastWriterWins.now(cellContent)

    val contentUpdate = content.update((colId, rowId), lastWriterWinsCellContent)

    Obrem(
      Spreadsheet(
        rowIds = rowIds, //.update(rowIdx, rowId).data,
        colIds = colIds, //.update(colIdx, colId).data,
        content = contentUpdate.data,
        keepRows = keepRowsUpdate.data,
        keepCols = keepColsUpdate.data
      ),
      contentUpdate.observed `union` keepRowsUpdate.observed `union` keepColsUpdate.observed `union` Dots.from(Array(rowId, colId/*, rowOperationId, colOperationId*/)),
      Dots.empty //Dots.from(oldKeepRows.values) `union` Dots.from(oldKeepCols.values)
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
