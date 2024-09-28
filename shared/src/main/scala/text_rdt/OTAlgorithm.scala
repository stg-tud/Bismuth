package text_rdt

// https://www3.ntu.edu.sg/scse/staff/czsun/projects/otfaq/

case class OTOperation(context: Int, inner: OperationType) {
    
}

 enum OperationType() {
    case Insert(i: Int, x: Char)
    case Delete(i: Int)
 }

final case class OTAlgorithm(val operations: Vector[OTOperation]) extends CollaborativeTextEditingAlgorithm {

  override def delete(i: Int): Unit = ???

  override def insert(i: Int, x: Char): Unit = ???

    
}