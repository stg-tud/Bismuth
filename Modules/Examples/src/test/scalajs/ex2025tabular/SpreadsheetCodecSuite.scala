package ex2025tabular

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import rdts.base.{Lattice, ReplicaId}
import webapps.ex2025tabular.lib.*
import webapps.ex2025tabular.lib.Spreadsheet.SpreadsheetCoordinate

class SpreadsheetCodecSuite extends munit.FunSuite {

  test("spreadsheets survive serialization") {
    val a = SpreadsheetDeltaAggregator(Spreadsheet[String](), ReplicaId.predefined("a"))
      .repeatEdit(3, _.addRow().delta)
      .repeatEdit(3, _.addColumn().delta)
    a.edit(_.editCell(SpreadsheetCoordinate(1.toRowIndex, 2.toColumnIndex), Some("hello")))
    a.edit(_.addRange(RangeId.gen, SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex), SpreadsheetCoordinate(1.toRowIndex, 1.toColumnIndex)))
    a.edit(_.removeRow(0.toRowIndex))

    val decoded = readFromString[Spreadsheet[String]](writeToString(a.current))
    assertEquals(decoded.toList, a.current.toList)
    assertEquals(decoded.numRows, a.current.numRows)
    assertEquals(decoded, a.current)

    // merging the decoded copy changes nothing, and further edits on a decoded copy merge back
    assertEquals(Lattice.merge(a.current, decoded).toList, a.current.toList)
    val b = SpreadsheetDeltaAggregator(decoded, ReplicaId.predefined("b"))
    val delta = b.editAndGetDelta()(_.editCell(SpreadsheetCoordinate(0.toRowIndex, 0.toColumnIndex), Some("from b")))
    a.accumulate(readFromString[Spreadsheet[String]](writeToString(delta)))
    assertEquals(a.current.toList, b.current.toList)
  }
}

class SampleSpreadsheetSuite extends munit.FunSuite {
  test("the sample sheet has 6 rows and 3 columns and identical for all replicas") {
    val a = webapps.ex2025tabular.app.SampleSpreadsheet()
    val b = webapps.ex2025tabular.app.SampleSpreadsheet()
    assertEquals(a.numRows, 6)
    assertEquals(a.numColumns, 3)
    assertEquals(Lattice.merge(a, b), a)
  }
}
