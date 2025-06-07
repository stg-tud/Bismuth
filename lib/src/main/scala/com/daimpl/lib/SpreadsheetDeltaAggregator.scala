package com.daimpl.lib

import rdts.base.LocalUid
import rdts.dotted.Dotted
import rdts.time.Dots

class SpreadsheetDeltaAggregator(
  var spreadsheet: Dotted[Spreadsheet]
)
{
  def edit(fn: Spreadsheet => rdts.time.Dots ?=> Dotted[Spreadsheet])(using LocalUid): SpreadsheetDeltaAggregator = {
    spreadsheet = spreadsheet.merge(spreadsheet.mod(fn))
    this
  }

  def visit(fn: Spreadsheet => Unit): SpreadsheetDeltaAggregator = {
    fn(spreadsheet.data)
    this
  }
}