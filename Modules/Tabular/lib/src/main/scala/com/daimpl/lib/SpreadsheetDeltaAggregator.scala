package com.daimpl.lib

import rdts.base.{Lattice, LocalUid, Uid}

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: Spreadsheet[S],
    private var replicaId: LocalUid
) {

  private type EditFunction = LocalUid ?=> Spreadsheet[S] => Spreadsheet[S]

  def editAndGetDelta(initialDelta: Spreadsheet[S] = Spreadsheet.empty[S])(fn: EditFunction)
      : Spreadsheet[S] = {
    val delta = fn(using replicaId)(spreadsheet)
    accumulate(delta)
    initialDelta.merge(delta)
  }

  def multiEditAndGetDelta(initialDelta: Spreadsheet[S] =
    Spreadsheet.empty[S])(sequentiallyAppliedEditFns: EditFunction*)
      : Spreadsheet[S] =
    sequentiallyAppliedEditFns.foldLeft(initialDelta) { editAndGetDelta(_)(_) }

  def edit(fn: EditFunction): SpreadsheetDeltaAggregator[S] = {
    editAndGetDelta()(fn)
    this
  }

  def repeatEdit(times: Int, fn: EditFunction): SpreadsheetDeltaAggregator[S] = {
    (0 until times) foreach { _ => edit(fn) }
    this
  }

  def accumulate(delta: Spreadsheet[S]): SpreadsheetDeltaAggregator[S] = {
    spreadsheet = spreadsheet.merge(delta)
    this
  }

  def visit(fn: Spreadsheet[S] => Unit): SpreadsheetDeltaAggregator[S] = {
    fn(spreadsheet)
    this
  }

  def current: Spreadsheet[S] = spreadsheet
}
