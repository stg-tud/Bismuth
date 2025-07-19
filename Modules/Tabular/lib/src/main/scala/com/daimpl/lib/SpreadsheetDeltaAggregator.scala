package com.daimpl.lib

import rdts.base.{Lattice, LocalUid, Uid}

class SpreadsheetDeltaAggregator[S: Lattice](
    private var spreadsheet: S,
    private var replicaId: LocalUid
) {
  def editAndGetDelta(fn: LocalUid ?=> S => S): S = {
    val delta = fn(using replicaId)(spreadsheet)
    spreadsheet = spreadsheet.merge(delta)
    delta
  }

  def edit(fn: LocalUid ?=> S => S): SpreadsheetDeltaAggregator[S] = {
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
