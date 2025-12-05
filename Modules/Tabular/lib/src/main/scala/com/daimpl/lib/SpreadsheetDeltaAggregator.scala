package com.daimpl.lib

import rdts.base.LocalUid

class SpreadsheetDeltaAggregator[S](
    private var spreadsheet: Spreadsheet[S],
    replicaId: LocalUid
) {

  private type EditFunction = LocalUid ?=> SpreadsheetOps[S] => Spreadsheet[S]
  private type UndoFunction = LocalUid ?=> Spreadsheet[S] => Spreadsheet[S]

  private var undoStack: List[UndoFunction] = Nil

  def editAndGetDelta(initialDelta: Spreadsheet[S] = Spreadsheet.empty[S])(fn: EditFunction)
      : Spreadsheet[S] = {
    val delta = fn(using replicaId)(spreadsheet)
    val recordingWrapper = new UndoRecordingSpreadsheet[S](
      spreadsheet,
      undoFn => undoStack = undoFn :: undoStack
    )
    val resultingOps = fn(using replicaId)(recordingWrapper)
    accumulate(resultingOps)
    initialDelta.merge(resultingOps)
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
