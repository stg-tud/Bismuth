package com.daimpl.app

import rdts.base.{LocalUid, Uid}
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted

def main(): Unit = {
  var list: Dotted[ReplicatedList[String]] = Dotted(ReplicatedList.empty)
  given LocalUid = LocalUid.gen()
  list = list `merge` list.mod(_.append("A"))
  list = list `merge` list.mod(_.append("B"))
  list = list `merge` list.mod(_.append("C"))
  println(list.data.toList)
  list = list `merge` list.mod(_.delete(1))
  list = list `merge` list.mod(_.delete(1))
  println(list.data.toList)

  /*val spreadsheet = SpreadsheetDeltaAggregator(Dotted(Spreadsheet()))
  val localUid = new LocalUid(new Uid("user1"))
  given LocalUid = localUid

  spreadsheet
    .edit(_.addRow())
    .edit(_.addRow())
    .edit(_.addColumn())
    .edit(_.addColumn())
    .edit(_.editCell(0, 0, "Hello World!"))
    .edit(_.editCell(1, 0, "Still here!"))
    .visit(_.printToConsole())
    .edit(_.removeRow(0))
    .edit(_.removeColumn(1))
    .visit(_.printToConsole())
    .edit(_.addRow())
    .edit(_.addColumn())
    .edit(_.purgeTombstones())
    .edit(_.editCell(0, 0, "1"))
    .edit(_.editCell(0, 1, "2"))
    .edit(_.editCell(1, 0, "3"))
    .edit(_.editCell(1, 1, "4"))
    .edit(_.insertRow(1))
    .visit(_.printToConsole())
    .edit(_.insertColumn(0))
    .visit(_.printToConsole())*/
}
