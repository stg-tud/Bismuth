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
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Edit = replica1.editAndGetDelta()(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta()(_.editCell(cellCoord, null))

    replica1.accumulate(replica2Deletion)
    replica2.accumulate(replica1Edit)

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
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Edit = replica1.editAndGetDelta()(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta()(_.removeRow(cellCoord.rowIdx))

    replica1.accumulate(replica2Deletion)
    replica2.accumulate(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(cellCoord).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent range addition and row removal of first row in range") {

    val numRows = 3
    val numCols = 3

    val rangeId = Uid("range")
    val range = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(2, 2))

    val removeRowIdx = 1

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Range = replica1.editAndGetDelta()(_.addRange(rangeId, range.from, range.to))

    val replica2Deletion = replica2.editAndGetDelta()(_.removeRow(removeRowIdx))

    replica1.accumulate(replica2Deletion)
    replica2.accumulate(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), None)
    assert(replica1.current.numRows == numRows - 1)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent range addition and row removal of internal row in range") {

    val numRows = 4
    val numCols = 3

    val rangeId = Uid("range")
    val range = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 2))

    val removeRowIdx = 2

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1Range = replica1.editAndGetDelta()(_.addRange(rangeId, range.from, range.to))

    val replica2Deletion = replica2.editAndGetDelta()(_.removeRow(removeRowIdx))

    replica1.accumulate(replica2Deletion)
    replica2.accumulate(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(range.from, range.to.copy(rowIdx = range.to.rowIdx - 1))))
    assert(replica1.current.numRows == numRows - 1)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrently move last column in range before the range beginning (1) and to the right (2)") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val range = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(rangeId, range.from, range.to))

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica1MoveLeft = replica1.editAndGetDelta()(_.moveColumn(range.to.colIdx, range.to.colIdx - 1))

    val replica2MoveRight = replica2.editAndGetDelta()(_.moveColumn(range.to.colIdx, range.to.colIdx + 2))

    replica1.accumulate(replica2MoveRight)
    replica2.accumulate(replica1MoveLeft)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(range.from, range.to.copy(colIdx = range.to.colIdx + 1))))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent column move and cell edit") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val range = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))

    val cellCoord = SpreadsheetCoordinate((range.from.rowIdx + range.to.rowIdx) / 2, range.to.colIdx)
    val cellText = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(rangeId, range.from, range.to))

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val replica2Edit = replica2.editAndGetDelta()(_.editCell(cellCoord, cellText))

    val replica1MoveLeft = replica1.editAndGetDelta()(_.moveColumn(range.to.colIdx, range.to.colIdx - 1))

    replica1.accumulate(replica2Edit)
    replica2.accumulate(replica1MoveLeft)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(range.from, range.to.copy(colIdx = range.to.colIdx - 1))))
    assertEquals(replica1.current.read(cellCoord.copy(colIdx = cellCoord.colIdx - 1)).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("three-way merge with mixed operations") {

    val numRows = 4
    val numCols = 4
    val coordinate = SpreadsheetCoordinate(2, 2)
    val text = "some text"

    val sharedInitialState = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
      .repeatEdit(numRows, _.addRow())
      .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("r1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("r2"))
    val replica3 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("r3"))

    val replica1Delta = replica1.editAndGetDelta()(_.editCell(coordinate, text))

    val replica2Delta = replica2.editAndGetDelta()(_.removeRow(coordinate.rowIdx))

    val replica3Delta = replica3.multiEditAndGetDelta()(
        _.moveColumn(coordinate.colIdx, 0),
        _.removeColumn(coordinate.colIdx + 1)
      )

    replica1.accumulate(replica2Delta).accumulate(replica3Delta)
    replica2.accumulate(replica3Delta).accumulate(replica1Delta)
    replica3.accumulate(replica1Delta).accumulate(replica2Delta)

    assertEquals(replica1.current, replica2.current)
    assertEquals(replica2.current, replica3.current)

    assert(replica1.current.read(SpreadsheetCoordinate(coordinate.rowIdx, 0)).toList.contains(text))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols - 1)
  }

  test("concurrent creation of overlapping ranges") {

    val numRows = 5
    val numCols = 5

    val sharedInitialState = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("sharedInitialState2"))
      .repeatEdit(numRows, _.addRow())
      .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("a"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("b"))

    val range1Id = Uid("range 1")
    val range1   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))
    val range2Id = Uid("range 2")
    val range2   = Range(SpreadsheetCoordinate(2, 2), SpreadsheetCoordinate(4, 4))

    val replica1Delta = replica1.editAndGetDelta()(_.addRange(range1Id, range1.from, range1.to))
    val replica2Delta = replica2.editAndGetDelta()(_.addRange(range2Id, range2.from, range2.to))

    replica1.accumulate(replica2Delta)
    replica2.accumulate(replica1Delta)

    assertEquals(replica1.current.getRange(range1Id), Some(range1))
    assertEquals(replica1.current.getRange(range2Id), Some(range2))
    assertEquals(replica1.current, replica2.current)
  }

  test("concurrent row insertion and cell edit at same row index") {
    val numRows = 2
    val numCols = 3
    val cellCoord = SpreadsheetCoordinate(1, 0)
    val cellText = "some text"

    val sharedInitialState = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
      .repeatEdit(numRows, _.addRow())
      .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("x"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("y"))

    val replica1Delta = replica1.editAndGetDelta()(_.insertRow(1))

    val replica2Delta = replica2.editAndGetDelta()(_.editCell(cellCoord, cellText))

    replica1.accumulate(replica2Delta)
    replica2.accumulate(replica1Delta)

    assertEquals(replica1.current.read(SpreadsheetCoordinate(2, 0)).toList, List(cellText))
    assertEquals(replica1.current.numRows, numRows + 1)
    assertEquals(replica1.current.numColumns, numCols)
    assertEquals(replica1.current, replica2.current)
  }
}
