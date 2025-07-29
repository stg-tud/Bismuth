package com.daimpl.lib

import com.daimpl.lib.Spreadsheet.{Range, SpreadsheetCoordinate}
import rdts.base.{LocalUid, Uid}

class SpreadsheetSuite extends munit.FunSuite {

  test("basic test") {
    val replica = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("replicaId"))
      .edit(_.addRow())
      .edit(_.addColumn())

    replica.edit(_.editCell(SpreadsheetCoordinate(0, 0), "test"))

    assertEquals(replica.current.read(SpreadsheetCoordinate(0, 0)).toList, List("test"))

    replica.edit(_.removeColumn(0))

    assert(replica.current.toList.flatten.isEmpty)
  }

  test("concurrent cell clear and edit") {

    val numRows = 3
    val numCols = 3

    val cellCoord = SpreadsheetCoordinate(1, 1)
    val cellText = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Edit = replica1.editAndGetDelta(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta(_.editCell(cellCoord, null))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(cellCoord).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent row deletion and cell edit") {

    val numRows = 3
    val numCols = 3

    val cellCoord = SpreadsheetCoordinate(1, 1)
    val cellText = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Edit = replica1.editAndGetDelta(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(cellCoord.rowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(cellCoord).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent range addition and row removal of first row in range") {

    val numRows = 3
    val numCols = 3

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord = SpreadsheetCoordinate(2, 2)

    val removeRowIdx = 1

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Range = replica1.editAndGetDelta(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(removeRowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), None)
    assert(replica1.current.numRows == numRows - 1)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent range addition and row removal of internal row in range") {

    val numRows = 4
    val numCols = 3

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord = SpreadsheetCoordinate(3, 2)

    val removeRowIdx = 2

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Range = replica1.editAndGetDelta(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(removeRowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(rangeFromCoord, rangeToCoord.copy(rowIdx = rangeToCoord.rowIdx - 1))))
    assert(replica1.current.numRows == numRows - 1)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrently move last column in range before the range beginning (1) and to the right (2)") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord = SpreadsheetCoordinate(3, 3)

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1MoveLeft = replica1.editAndGetDelta(_.moveColumn(rangeToCoord.colIdx, rangeFromCoord.colIdx - 1))

    val replica2MoveRight = replica2.editAndGetDelta(_.moveColumn(rangeToCoord.colIdx, rangeToCoord.colIdx + 2))

    replica1.merge(replica2MoveRight)
    replica2.merge(replica1MoveLeft)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(rangeFromCoord, rangeToCoord.copy(colIdx = rangeToCoord.colIdx + 1))))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent column move and cell edit") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord = SpreadsheetCoordinate(3, 3)

    val cellCoord = SpreadsheetCoordinate((rangeFromCoord.rowIdx + rangeToCoord.rowIdx) / 2, rangeToCoord.colIdx)
    val cellText = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica2Edit = replica2.editAndGetDelta(_.editCell(cellCoord, cellText))

    val replica1MoveLeft = replica1.editAndGetDelta(_.moveColumn(rangeToCoord.colIdx, rangeToCoord.colIdx - 1))

    replica1.merge(replica2Edit)
    replica2.merge(replica1MoveLeft)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(rangeFromCoord, rangeToCoord.copy(colIdx = rangeToCoord.colIdx - 1))))
    assertEquals(replica1.current.read(cellCoord.copy(colIdx = cellCoord.colIdx - 1)).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }
}
