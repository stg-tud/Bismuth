package com.daimpl.lib

import rdts.base.LocalUid

class BasicTest extends munit.FunSuite {

  test("basic test") {
    given LocalUid = LocalUid.predefined("a")
    val agg        = SpreadsheetDeltaAggregator(Spreadsheet())
      .edit(_.addRow())
      .edit(_.addColumn())

    agg.edit(_.editCell(0, 0, "test"))

    assertEquals(agg.current.read(0, 0), Some("test"))

  }

}
