package com.daimpl.lib

import rdts.base.{Lattice, LocalUid}
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dots

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: Obrem[S]
) {
  def edit(fn: Dots ?=> S => Obrem[S])(using LocalUid)(using Lattice[Obrem[S]]): SpreadsheetDeltaAggregator[S] =
    spreadsheet = spreadsheet.merge(spreadsheet.mod(fn))
    this

  def visit(fn: S => Unit): SpreadsheetDeltaAggregator[S] =
    fn(spreadsheet.data)
    this

  def current: S = spreadsheet.data
}
