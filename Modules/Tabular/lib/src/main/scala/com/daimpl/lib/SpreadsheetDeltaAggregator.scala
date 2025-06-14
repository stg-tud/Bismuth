package com.daimpl.lib

import rdts.base.Lattice

class SpreadsheetDeltaAggregator[S: Lattice](
    private var spreadsheet: S
) {
  def editAndGetDelta(fn: S => S): S = {
    val delta = fn(spreadsheet)
    spreadsheet = spreadsheet.merge(delta)
    delta
  }

  def edit(fn: S => S): SpreadsheetDeltaAggregator[S] = {
    editAndGetDelta(fn)
    this
  }

  def merge(delta: S): SpreadsheetDeltaAggregator[S] = {
    spreadsheet = spreadsheet.merge(delta)
    this
  }

  def visit(fn: S => Unit): SpreadsheetDeltaAggregator[S] = {
    fn(spreadsheet)
    this
  }

  def current: S = spreadsheet

}
