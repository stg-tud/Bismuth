package datatypes

import dag.{HashDAG, SyncRequest}
import riblt.{CodedSymbol, RIBLT}
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}

trait Replica[T, R <: Replica[T, R]] { self: R =>
  def hashDAG: HashDAG[T]

  def riblt: RIBLT[String]

  def merge(other: R): R

  def empty: R

  def withHashDAG(hashDAG: HashDAG[T]): R

  def produceNextCodedSymbols(count: Int = 1): List[CodedSymbol[String]] =
    this.riblt.produceNextCodedSymbols(count)

  def addCodedSymbols(codedSymbols: List[CodedSymbol[String]]): R =
    for codedSymbol <- codedSymbols do
      this.riblt.addCodedSymbol(codedSymbol)

    self

  def sendSyncRequest: SyncRequest[T] =
    if riblt.isDecoded then
      val ids = riblt.localSymbols.map(s => s.value)

      SyncRequest(
        hashDAG.withQueue(ids.map(id => hashDAG.events(id)).toSet),
        riblt.remoteSymbols.map(s => s.value).toSet
      )
    else
      SyncRequest(hashDAG.empty, Set.empty)

  def receiveSyncRequest(syncRequest: SyncRequest[T]): (R, R) =
    (
      this.merge(this.empty.withHashDAG(syncRequest.hashDAG)),
      this.empty.withHashDAG(hashDAG.empty.withQueue(syncRequest.requestedEvents.map(id => hashDAG.events(id))))
    )

}  