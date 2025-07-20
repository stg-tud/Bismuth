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

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addColumn())
        .edit(_.addColumn())
        .edit(_.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val cellCoord = SpreadsheetCoordinate(1, 1)
    val cellText = "some text"

    val replica1Edit = replica1.editAndGetDelta(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta(_.editCell(cellCoord, null))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(cellCoord).toList, List(cellText))
    assert(replica1.current.numColumns == 3)
    assert(replica1.current.numRows == 3)

    val replica1Purged = replica1.edit(_.purgeTombstones())
    val replica2Purged = replica2.edit(_.purgeTombstones())

    assertEquals(replica1Purged.current, replica2Purged.current)
  }

  test("concurrent row deletion and cell edit") {

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addColumn())
        .edit(_.addColumn())
        .edit(_.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val cellCoord = SpreadsheetCoordinate(1, 1)
    val cellText = "some text"

    val replica1Edit = replica1.editAndGetDelta(_.editCell(cellCoord, cellText))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(cellCoord.rowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(cellCoord).toList, List(cellText))
    assert(replica1.current.numColumns == 3)
    assert(replica1.current.numRows == 3)

    val replica1Purged = replica1.edit(_.purgeTombstones())
    val replica2Purged = replica2.edit(_.purgeTombstones())

    assertEquals(replica1Purged.current, replica2Purged.current)
  }

  test("remove first row in range") {

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addColumn())
        .edit(_.addColumn())
        .edit(_.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord   = SpreadsheetCoordinate(2, 2)

    val removeRowIdx = 1

    val replica1Range = replica1.editAndGetDelta(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(removeRowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), None)
    assert(replica1.current.numColumns == 3)
    assert(replica1.current.numRows == 2)

    val replica1Purged = replica1.edit(_.purgeTombstones())
    val replica2Purged = replica2.edit(_.purgeTombstones())

    assertEquals(replica1Purged.current, replica2Purged.current)
  }

  test("remove middle row in range") {

    val sharedInitialState =
      SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("shared"))
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addRow())
        .edit(_.addColumn())
        .edit(_.addColumn())
        .edit(_.addColumn())

    val replica1 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica1"))
    val replica2 = SpreadsheetDeltaAggregator(sharedInitialState.current, LocalUid.predefined("replica2"))

    val rangeId = Uid("range")
    val rangeFromCoord = SpreadsheetCoordinate(1, 1)
    val rangeToCoord = SpreadsheetCoordinate(3, 2)

    val removeRowIdx = 2

    val replica1Range = replica1.editAndGetDelta(_.addRange(rangeId, rangeFromCoord, rangeToCoord))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(removeRowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Range)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.getRange(rangeId), Some(Range(SpreadsheetCoordinate(1, 1), SpreadsheetCoordinate(2, 2))))
    assert(replica1.current.numColumns == 3)
    assert(replica1.current.numRows == 3)

    val replica1Purged = replica1.edit(_.purgeTombstones())
    val replica2Purged = replica2.edit(_.purgeTombstones())

    assertEquals(replica1Purged.current, replica2Purged.current)
  }
}
