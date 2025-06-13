package com.daimpl.lib

import rdts.base.{Lattice, LocalUid}
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dots

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: Obrem[S]
) {
  def editAndGetDelta(fn: Dots ?=> S => Obrem[S])(using LocalUid, Lattice[Obrem[S]]): Obrem[S] = {
    val delta = spreadsheet.mod(fn)
    spreadsheet = spreadsheet.merge(delta)
    delta
  }

  def edit(fn: Dots ?=> S => Obrem[S])(using LocalUid, Lattice[Obrem[S]]): SpreadsheetDeltaAggregator[S] = {
    editAndGetDelta(fn)
    this
  }

  def merge(delta: Obrem[S])(using Lattice[Obrem[S]]): SpreadsheetDeltaAggregator[S] = {
    spreadsheet = spreadsheet.merge(delta)
    this
  }

  def visit(fn: S => Unit): SpreadsheetDeltaAggregator[S] = {
    fn(spreadsheet.data)
    this
  }

  def current: S = spreadsheet.data

  def getObrem: Obrem[S] = spreadsheet
}
