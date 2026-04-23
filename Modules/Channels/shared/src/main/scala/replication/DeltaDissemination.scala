package replication

trait DeltaDissemination[State] {
  def applyDelta(delta: State): Unit
}

trait DeltaDisseminationFactory[State] {
  def bind(receiveCallback: State => Unit): DeltaDissemination[State]
}
