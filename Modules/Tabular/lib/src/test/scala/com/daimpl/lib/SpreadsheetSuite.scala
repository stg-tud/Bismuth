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
    val cellText  = "some text"

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
    val cellText  = "some text"

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
    val range   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(2, 2))

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
    val range   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 2))

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

    assertEquals(
      replica1.current.getRange(rangeId),
      Some(Range(range.from, range.to.copy(rowIdx = range.to.rowIdx - 1)))
    )
    assert(replica1.current.numRows == numRows - 1)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrently move last column in range before the range beginning (1) and to the right (2)") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val range   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))

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

    assertEquals(
      replica1.current.getRange(rangeId),
      Some(Range(range.from, range.to.copy(colIdx = range.to.colIdx + 1)))
    )
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent deletes of same columns/rows in presence of nested ranges") {

    val numRows = 6
    val numCols = 6

    val range1Id = Uid("enclosing range")
    val range1   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(4, 5))

    val range2Id = Uid("inner range")
    val range2   = Range(SpreadsheetCoordinate(2, 2), SpreadsheetCoordinate(3, 3))

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(range1Id, range1.from, range1.to))
        .edit(_.addRange(range2Id, range2.from, range2.to))
        .current

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("r1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("r2"))

    var replica2Delta = replica2.editAndGetDelta()(_.removeColumn(2))
    val replica1Delta = replica1.multiEditAndGetDelta()(
      _.removeRow(2),
      _.removeColumn(2)
    )
    replica2Delta = replica2.editAndGetDelta(replica2Delta)(_.removeRow(2))

    replica1.accumulate(replica2Delta)
    replica2.accumulate(replica1Delta)

    assertEquals(replica1.current, replica2.current)

    val range1Result = replica1.current.getRange(range1Id).get
    assertEquals(range1Result, Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 4)))

    val range2Result = replica1.current.getRange(range2Id).get
    assertEquals(range2Result, Range(SpreadsheetCoordinate(2, 2), SpreadsheetCoordinate(2, 2)))
  }

  test("concurrent column move and cell edit") {

    val numRows = 5
    val numCols = 5

    val rangeId = Uid("range")
    val range   = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))

    val cellCoord = SpreadsheetCoordinate((range.from.rowIdx + range.to.rowIdx) / 2, range.to.colIdx)
    val cellText  = "some text"

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

    assertEquals(
      replica1.current.getRange(rangeId),
      Some(Range(range.from, range.to.copy(colIdx = range.to.colIdx - 1)))
    )
    assertEquals(replica1.current.read(cellCoord.copy(colIdx = cellCoord.colIdx - 1)).toList, List(cellText))
    assert(replica1.current.numRows == numRows)
    assert(replica1.current.numColumns == numCols)
  }

  test("concurrent column move and column removal: column is kept") {

    val numRows        = 2
    val numCols        = 3
    val cellCoord      = SpreadsheetCoordinate(1, 1)
    val cellText       = "some text"
    val movedCol       = cellCoord.colIdx
    val movedColTarget = cellCoord.colIdx + 1

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.editCell(cellCoord, cellText))
        .current

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("replica 1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("replica 2"))

    val replica1Delta = replica1.editAndGetDelta()(_.moveColumn(movedCol, movedColTarget + 1))
    val replica2Delta = replica2.editAndGetDelta()(_.removeColumn(movedCol))

    replica1.accumulate(replica2Delta)
    replica2.accumulate(replica1Delta)

    assertEquals(replica1.current, replica2.current)
    assertEquals(replica1.current.numRows, numRows)
    assertEquals(replica1.current.numColumns, numCols)
    assertEquals(replica1.current.read(cellCoord.copy(colIdx = movedColTarget)).toList, List(cellText))
  }

  test("concurrent cell edits at same coordinates: later edit wins") {

    val numRows = 3
    val numCols = 3

    val cell1Coord = SpreadsheetCoordinate(1, 1)
    val cell2Coord = SpreadsheetCoordinate(1, 1)

    val edit1Text = "write 1"
    val edit2Text = "write 2"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .current

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("s1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("s2"))

    var replica1Delta = replica1.editAndGetDelta()(_.editCell(cell1Coord, edit1Text))
    var replica2Delta = replica2.editAndGetDelta()(_.editCell(cell2Coord, edit1Text))

    replica2Delta = replica2.editAndGetDelta(replica2Delta)(_.editCell(cell1Coord, edit2Text))
    replica1Delta = replica1.editAndGetDelta(replica1Delta)(_.editCell(cell2Coord, edit2Text))

    replica1.accumulate(replica2Delta)
    replica2.accumulate(replica1Delta)

    assertEquals(replica1.current, replica2.current)
    assertEquals(replica1.current.numRows, numRows)
    assertEquals(replica1.current.numColumns, numCols)
    assertEquals(replica1.current.read(cell1Coord).toList, List(edit2Text))
    assertEquals(replica1.current.read(cell2Coord).toList, List(edit2Text))
  }

  test("three-way merge with mixed operations 1") {

    val numRows    = 4
    val numCols    = 4
    val coordinate = SpreadsheetCoordinate(2, 2)
    val text       = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
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

  test("three-way merge with mixed operations 2") {

    val numRows       = 6
    val numCols       = 6
    val editCellCoord = SpreadsheetCoordinate(2, 1)
    val moveCellCoord = SpreadsheetCoordinate(5, 4)
    val editCellText  = "some text by replica 1"
    val moveCellText  = "some text by replica 2"
    val rangeId       = Uid("range")
    val range         = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())
        .edit(_.addRange(rangeId, range.from, range.to))

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica 1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica 2"))
    val replica3 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica 3"))

    val replica1Delta = replica1.multiEditAndGetDelta()(
      _.editCell(editCellCoord, editCellText),
      _.moveColumn(moveCellCoord.colIdx, 0)
    )

    val replica2Delta = replica2.multiEditAndGetDelta()(
      _.editCell(moveCellCoord, moveCellText),
      _.removeRow(editCellCoord.rowIdx),
      _.insertRow(0)
    )

    val replica3Delta = replica3.multiEditAndGetDelta()(
      _.removeColumn(editCellCoord.colIdx),
      _.removeRange(rangeId)
    )

    replica1.accumulate(replica2Delta).accumulate(replica3Delta)
    replica2.accumulate(replica3Delta).accumulate(replica1Delta)
    replica3.accumulate(replica1Delta).accumulate(replica2Delta)

    assertEquals(replica1.current, replica2.current)
    assertEquals(replica2.current, replica3.current)

    assert(replica1.current.numRows == numRows + 1)
    assert(replica1.current.numColumns == numCols)

    assertEquals(
      replica1.current.read(SpreadsheetCoordinate(editCellCoord.rowIdx + 1, editCellCoord.colIdx + 1)).toList,
      List(editCellText)
    )

    assertEquals(replica1.current.read(SpreadsheetCoordinate(moveCellCoord.rowIdx + 1, 0)).toList, List(moveCellText))

    assertEquals(replica1.current.getRange(rangeId), None)
  }

  test("three-way merge with mixed operations 3") {

    val sharedInitialState = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("mix0"))
      .repeatEdit(4, _.addRow())
      .repeatEdit(4, _.addColumn())
      .current

    val cellCoord = SpreadsheetCoordinate(2, 2)
    val cellText  = "X"

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("a"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("b"))
    val replica3 = SpreadsheetDeltaAggregator(sharedInitialState, LocalUid.predefined("c"))

    val replica1Delta = replica1.multiEditAndGetDelta()(
      _.removeRow(1),
      _.editCell(cellCoord, cellText)
    )
    val replica2Delta = replica2.multiEditAndGetDelta()(
      _.moveColumn(2, 0),
      _.removeColumn(3)
    )
    val replica3Delta = replica3.multiEditAndGetDelta()(
      _.removeRow(1),
      _.moveColumn(2, 0)
    )

    replica1.accumulate(replica2Delta).accumulate(replica3Delta)
    replica2.accumulate(replica3Delta).accumulate(replica1Delta)
    replica3.accumulate(replica1Delta).accumulate(replica2Delta)

    assertEquals(replica1.current, replica2.current)
    assertEquals(replica2.current, replica3.current)

    assertEquals(replica1.current.read(SpreadsheetCoordinate(2, 0)).toList, List(cellText))
  }

  test("concurrent creation of overlapping ranges") {

    val numRows = 5
    val numCols = 5

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("sharedInitialState2"))
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

  test("create, delete, re-create range") {

    val numRows = 5
    val numCols = 5

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("sharedInitialState2"))
        .repeatEdit(numRows, _.addRow())
        .repeatEdit(numCols, _.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("a"))

    val rangeId = Uid("range")
    val range1  = Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(3, 3))
    val range2  = Range(SpreadsheetCoordinate(2, 2), SpreadsheetCoordinate(4, 4))

    replica1.multiEditAndGetDelta()(
      _.addRange(rangeId, range1.from, range1.to),
      _.removeRange(rangeId),
      _.addRange(rangeId, range2.from, range2.to)
    )

    assertEquals(replica1.current.getRange(rangeId), Some(range2))
  }

  test("concurrent row insertion and cell edit at same row index") {
    val numRows   = 2
    val numCols   = 3
    val cellCoord = SpreadsheetCoordinate(1, 0)
    val cellText  = "some text"

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared initial state"))
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
