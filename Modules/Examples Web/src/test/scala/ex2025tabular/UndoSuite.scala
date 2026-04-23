package ex2025tabular

import rdts.base.{LocalUid, Uid}
import webapps.ex2025tabular.lib.Spreadsheet.{Range, SpreadsheetCoordinate}
import webapps.ex2025tabular.lib.*

class UndoSuite extends munit.FunSuite {

  private val cellCoord = SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex)

  private def emptySheetWithOneCell(): Spreadsheet[String] =
    SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("setup"))
      .edit(_.addRow().delta, allowUndo = false)
      .edit(_.addColumn().delta, allowUndo = false)
      .current

  private def assertCellValues(aggregator: SpreadsheetDeltaAggregator[String], expected: Set[String]): Unit =
    assertEquals(aggregator.current.read(cellCoord).toList.toSet, expected)

  test("replicated edit undos restore previous values in order") {
    val initialSheet = emptySheetWithOneCell()

    val replicaA = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaA"))
    val replicaB = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaB"))

    val initDelta = replicaA.editAndGetDelta()(_.editCell(cellCoord, "init"))
    replicaB.accumulate(initDelta)
    assertCellValues(replicaA, Set("init"))
    assertCellValues(replicaB, Set("init"))

    val replicaAEdit = replicaA.editAndGetDelta()(_.editCell(cellCoord, "a"))
    val replicaBEdit = replicaB.editAndGetDelta()(_.editCell(cellCoord, "b"))

    replicaA.accumulate(replicaBEdit)
    replicaB.accumulate(replicaAEdit)
    assertCellValues(replicaA, Set("a", "b"))
    assertCellValues(replicaB, Set("a", "b"))

    val replicaBUndo = replicaB.undoAndGetDelta().get
    assertCellValues(replicaB, Set("init", "a"))
    replicaA.accumulate(replicaBUndo)
    assertCellValues(replicaA, Set("init", "a"))

    val replicaAUndo1 = replicaA.undoAndGetDelta().get
    assertCellValues(replicaA, Set("init"))
    replicaB.accumulate(replicaAUndo1)
    assertCellValues(replicaB, Set("init"))

    val replicaAUndo2 = replicaA.undoAndGetDelta().get
    assertCellValues(replicaA, Set.empty)
    replicaB.accumulate(replicaAUndo2)
    assertCellValues(replicaB, Set.empty)
  }

  test("undo edit restores all previous conflicting values") {
    val initialSheet = emptySheetWithOneCell()

    val replica1 = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replica2"))

    val replica1Edit = replica1.editAndGetDelta()(_.editCell(cellCoord, "a"))
    val replica2Edit = replica2.editAndGetDelta()(_.editCell(cellCoord, "b"))

    replica1.accumulate(replica2Edit)

    assertCellValues(replica1, Set("a", "b"))

    val undoingReplica = SpreadsheetDeltaAggregator(replica1.current, LocalUid.predefined("undo"))
    undoingReplica.edit(_.editCell(cellCoord, "c"))

    assertCellValues(undoingReplica, Set("c"))
    assert(undoingReplica.undo())
    assertCellValues(undoingReplica, Set("a", "b"))
  }

  test("undo add and remove row/column restores structure") {
    val aggregator = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("replica"))

    aggregator.edit(_.addRow().delta)
    aggregator.edit(_.addColumn().delta)
    assertEquals(aggregator.current.numRows, 1)
    assertEquals(aggregator.current.numColumns, 1)

    aggregator.edit(_.addRow().delta)
    aggregator.edit(_.addColumn().delta)
    assertEquals(aggregator.current.numRows, 2)
    assertEquals(aggregator.current.numColumns, 2)

    assert(aggregator.undo())
    assertEquals(aggregator.current.numRows, 2)
    assertEquals(aggregator.current.numColumns, 1)

    assert(aggregator.undo())
    assertEquals(aggregator.current.numRows, 1)
    assertEquals(aggregator.current.numColumns, 1)

    aggregator.edit(_.removeRow(0.toRowIndex))
    aggregator.edit(_.removeColumn(0.toColumnIndex))
    assertEquals(aggregator.current.numRows, 0)
    assertEquals(aggregator.current.numColumns, 0)

    assert(aggregator.undo())
    assertEquals(aggregator.current.numRows, 0)
    assertEquals(aggregator.current.numColumns, 1)

    assert(aggregator.undo())
    assertEquals(aggregator.current.numRows, 1)
    assertEquals(aggregator.current.numColumns, 1)
  }

  test("undo removed column restores it locally and across replicas") {
    val initialSheet = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("setup"))
      .edit(_.addRow().delta, allowUndo = false)
      .repeatEdit(2, _.addColumn().delta, allowUndo = false)
      .current

    val replicaA = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaA"))
    val replicaB = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaB"))

    val removeDelta = replicaA.editAndGetDelta()(_.removeColumn(0.toColumnIndex))
    replicaB.accumulate(removeDelta)

    assertEquals(replicaA.current.numColumns, 1)
    assertEquals(replicaB.current.numColumns, 1)

    val undoDelta = replicaA.undoAndGetDelta().get
    assertEquals(replicaA.current.numColumns, 2)

    replicaB.accumulate(undoDelta)
    assertEquals(replicaB.current.numColumns, 2)
  }

  test("undo removed column is a no-op if merge already kept the column") {
    val initialSheet = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("setup"))
      .edit(_.addRow().delta, allowUndo = false)
      .repeatEdit(2, _.addColumn().delta, allowUndo = false)
      .current

    val replicaA = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaA"))
    val replicaB = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaB"))

    val removeDelta = replicaA.editAndGetDelta()(_.removeColumn(0.toColumnIndex))
    val keepAliveDelta = replicaB.editAndGetDelta()(_.editCell(SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex), "kept"))

    replicaA.accumulate(keepAliveDelta)
    replicaB.accumulate(removeDelta)

    assertEquals(replicaA.current.numColumns, 2)
    assertEquals(replicaB.current.numColumns, 2)
    assertEquals(replicaA.current.read(SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex)).toList, List("kept"))

    val beforeUndo = replicaA.current
    val undoDelta = replicaA.undoAndGetDelta().get

    assertEquals(replicaA.current, beforeUndo)

    replicaB.accumulate(undoDelta)
    assertEquals(replicaB.current, beforeUndo)
  }

  test("undo inserted column is a no-op if merge already deleted that column") {
    val initialSheet = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("setup"))
      .edit(_.addRow().delta, allowUndo = false)
      .edit(_.addColumn().delta, allowUndo = false)
      .current

    val replicaA = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaA"))
    val replicaB = SpreadsheetDeltaAggregator(initialSheet, LocalUid.predefined("replicaB"))

    val insertDelta = replicaA.editAndGetDelta()(_.insertColumn(0.toColumnIndex).delta)
    replicaB.accumulate(insertDelta)

    assertEquals(replicaA.current.numColumns, 2)
    assertEquals(replicaB.current.numColumns, 2)

    val insertedColumnId = replicaA.current.listColumnIds.head
    val insertedColumnIndex = replicaB.current.getColIndex(insertedColumnId).get
    val removeInsertedDelta = replicaB.editAndGetDelta()(_.removeColumn(insertedColumnIndex))
    replicaA.accumulate(removeInsertedDelta)

    assertEquals(replicaA.current.numColumns, 1)
    assertEquals(replicaB.current.numColumns, 1)

    val beforeUndo = replicaA.current
    val undoDelta = replicaA.undoAndGetDelta().get

    assertEquals(replicaA.current, beforeUndo)

    replicaB.accumulate(undoDelta)
    assertEquals(replicaB.current, beforeUndo)
  }

  test("undo row and column moves restores order") {
    val aggregator = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("replica"))
      .repeatEdit(3, _.addRow().delta)
      .repeatEdit(3, _.addColumn().delta)

    val initialRowIds = aggregator.current.listRowIds
    val initialColIds = aggregator.current.listColumnIds

    aggregator.edit(_.moveRow(0.toRowIndex, 3.toRowIndex))
    aggregator.edit(_.moveColumn(0.toColumnIndex, 3.toColumnIndex))

    assertEquals(aggregator.current.listRowIds, List(initialRowIds(1), initialRowIds(2), initialRowIds(0)))
    assertEquals(aggregator.current.listColumnIds, List(initialColIds(1), initialColIds(2), initialColIds(0)))

    assert(aggregator.undo())
    assertEquals(aggregator.current.listColumnIds, initialColIds)

    assert(aggregator.undo())
    assertEquals(aggregator.current.listRowIds, initialRowIds)
  }

  test("undo range add and remove restores range state") {
    val rangeId = RangeId(Uid("undo-range"))
    val range = Range(SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex), SpreadsheetCoordinate(1.toRowIndex, 1.toColumnIndex))

    val aggregator = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("replica"))
      .repeatEdit(2, _.addRow().delta)
      .repeatEdit(2, _.addColumn().delta)

    aggregator.edit(_.addRange(rangeId, range.from, range.to))
    assertEquals(aggregator.current.getRange(rangeId), Some(range))

    assert(aggregator.undo())
    assertEquals(aggregator.current.getRange(rangeId), None)

    aggregator.edit(_.addRange(rangeId, range.from, range.to))
    aggregator.edit(_.removeRange(rangeId))
    assertEquals(aggregator.current.getRange(rangeId), None)

    assert(aggregator.undo())
    assertEquals(aggregator.current.getRange(rangeId), Some(range))
  }
}
