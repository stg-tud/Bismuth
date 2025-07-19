package com.daimpl.lib

import rdts.base.LocalUid

class BasicTest extends munit.FunSuite {

  test("basic test") {
    val replica = SpreadsheetDeltaAggregator(Spreadsheet[String](), LocalUid.predefined("replicaId"))
      .edit(_.addRow())
      .edit(_.addColumn())

    replica.edit(_.editCell(0, 0, "test"))

    assertEquals(replica.current.read(0, 0).toList, List("test"))

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

      val (rowIdx, colIdx) = (1, 1)
      val cellText = "some text"

      val replica1Edit = replica1.editAndGetDelta(_.editCell(rowIdx, colIdx, cellText))

      val replica2Deletion = replica2.editAndGetDelta(_.editCell(rowIdx, colIdx, null))

      replica1.merge(replica2Deletion)
      replica2.merge(replica1Edit)

      assertEquals(replica1.current, replica2.current)

      assertEquals(replica1.current.read(rowIdx, colIdx).toList, List(cellText))
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

    val (rowIdx, colIdx) = (1, 1)
    val cellText = "some text"

    val replica1Edit = replica1.editAndGetDelta(_.editCell(rowIdx, colIdx, cellText))

    val replica2Deletion = replica2.editAndGetDelta(_.removeRow(rowIdx))

    replica1.merge(replica2Deletion)
    replica2.merge(replica1Edit)

    assertEquals(replica1.current, replica2.current)

    assertEquals(replica1.current.read(rowIdx, colIdx).toList, List(cellText))
    assert(replica1.current.numColumns == 3)
    assert(replica1.current.numRows == 3)

    val replica1Purged = replica1.edit(_.purgeTombstones())
    val replica2Purged = replica2.edit(_.purgeTombstones())

    assertEquals(replica1Purged.current, replica2Purged.current)
  }
}
