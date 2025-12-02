package com.daimpl.lib

import rdts.base.LocalUid

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: Spreadsheet[S],
    replicaId: LocalUid
) {

  private type EditFunction = LocalUid ?=> Spreadsheet[S] => Spreadsheet[S]

  private var undoStack: List[EditFunction] = Nil

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

  def editWithUndo(doEdit: EditFunction, undoEdit: EditFunction): SpreadsheetDeltaAggregator[S] = {
    editAndGetDelta()(doEdit)
    undoStack = undoEdit :: undoStack
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

  def undoAndGetDelta(initialDelta: Spreadsheet[S] = Spreadsheet.empty[S]): Option[Spreadsheet[S]] = {
    undoStack match {
      case undoFn :: rest =>
        undoStack = rest
        val delta = undoFn(using replicaId)(spreadsheet)
        accumulate(delta)
        Some(initialDelta.merge(delta))
      case Nil => None
    }
  }

  def undo(): Boolean = undoAndGetDelta().isDefined

  def canUndo: Boolean = undoStack.nonEmpty

  def visit(fn: Spreadsheet[S] => Unit): SpreadsheetDeltaAggregator[S] = {
    fn(spreadsheet)
    this
  }

  def current: Spreadsheet[S] = spreadsheet
}
