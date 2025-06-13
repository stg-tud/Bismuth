package com.daimpl.lib

import rdts.base.{Lattice, LocalUid}
import rdts.time.Dots

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: S
) {
  def editAndGetDelta(fn: S => S)(using LocalUid, Lattice[S]): S = {
    val delta = fn(spreadsheet)
    spreadsheet = spreadsheet.merge(delta)
    delta
  }

  def edit(fn: S => S)(using LocalUid, Lattice[S]): SpreadsheetDeltaAggregator[S] = {
    editAndGetDelta(fn)
    this
  }

  def merge(delta: S)(using Lattice[S]): SpreadsheetDeltaAggregator[S] = {
    spreadsheet = spreadsheet.merge(delta)
    this
  }

  def visit(fn: S => Unit): SpreadsheetDeltaAggregator[S] = {
    fn(spreadsheet)
    this
  }

  def current: S = spreadsheet

  def getObrem: S = spreadsheet
}
