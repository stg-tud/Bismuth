package text_rdt

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// https://www3.ntu.edu.sg/scse/staff/czsun/projects/otfaq/#_Toc321146152
// COT would be way to complicated, choose something simpler
// dOPT
// https://dl.acm.org/doi/pdf/10.1145/289444.289469

// https://www3.ntu.edu.sg/scse/staff/czsun/projects/otfaq/#_Toc321146146

// https://arxiv.org/pdf/1905.01302
// 3.1.3 Server-based versus Distributed OT

// The context needs to represent a context vector. This means for every replicating site it stores which operations O_0 to O_i have been executed. https://dl.acm.org/doi/pdf/10.1145/1180875.1180918
// 2.7.How to represent operation context in OT design? 
case class OTOperation(replica: RID, inner: OperationType, context: CausalID) {
    
}

def inclusionTransform(operationToTransform: OTOperation, operationToTransformAgainst: OTOperation): OTOperation = {
  //println(s"inclusion transforming $operationToTransform against $operationToTransformAgainst")
  val result = inclusionTransformInternal(operationToTransform, operationToTransformAgainst)
  //println(s"inclusion transformed $operationToTransform against $operationToTransformAgainst to $result")
  // possibly the reverse does not hold
  //assert(operationToTransform == exclusionTransformInternal(result, operationToTransformAgainst), s"IT($operationToTransform, $operationToTransformAgainst) -> $result -> ET($result, $operationToTransformAgainst) -> ${exclusionTransformInternal(result, operationToTransformAgainst)}")
  result
}

// 2.17.    What are the pre-/post-conditions for transformation functions?
def inclusionTransformInternal(operationToTransform: OTOperation, operationToTransformAgainst: OTOperation): OTOperation = {
  assert(operationToTransform.context == operationToTransformAgainst.context, s"${operationToTransform.context} == ${operationToTransformAgainst.context}")
  val result = ((operationToTransform.replica, operationToTransform.inner), (operationToTransformAgainst.replica, operationToTransformAgainst.inner)) match {
    case Tuple2((oReplica, OperationType.Identity), other) => (oReplica, OperationType.Identity)
    case Tuple2(other, (bReplica, OperationType.Identity)) => other
    case Tuple2((oReplica, OperationType.Insert(oI, oX)), (bReplica, OperationType.Insert(bI, bX))) => if (oI < bI || (oI == bI && oReplica < bReplica)) {
      (oReplica, OperationType.Insert(oI, oX))
    } else {
      (oReplica, OperationType.Insert(oI + 1, oX))
    }
    case Tuple2((oReplica, OperationType.Insert(oI, oX)), (bReplica, OperationType.Delete(bI))) => if (oI <= bI) {
      (oReplica, OperationType.Insert(oI, oX))
    } else {
      (oReplica, OperationType.Insert(oI - 1, oX))
    }
    case Tuple2((oReplica, OperationType.Delete(oI)), (bReplica, OperationType.Insert(bI, bX))) => if (oI < bI) {
      (oReplica, OperationType.Delete(oI))
    } else {
      (oReplica, OperationType.Delete(oI + 1))
    }
    case Tuple2((oReplica, OperationType.Delete(oI)), (bReplica, OperationType.Delete(bI))) => if (oI < bI) {
      (oReplica, OperationType.Delete(oI))
    } else if (oI > bI) {
      (oReplica, OperationType.Delete(oI - 1))
    } else {
      (oReplica, OperationType.Identity)
    }
  }
  OTOperation(result._1, result._2, operationToTransformAgainst.context.clone().addOne(operationToTransformAgainst.replica, operationToTransformAgainst.context.getOrElse(operationToTransformAgainst.replica, 0) + 1))
}

enum OperationType() {
  case Insert(i: Int, x: Char)
  case Delete(i: Int)
  case Identity
}

final case class OTAlgorithm(replicaId: String, val operations: Vector[OTOperation]) {

  val operationsPerSite: mutable.HashMap[RID, mutable.ArrayBuffer[OTOperation]] = mutable.HashMap()

  val causalBroadcast = CausalBroadcast[OTOperation](replicaId, false)

  val text: StringBuilder = StringBuilder()

}

// TODO FIXME maybe disable message batching for this?
object OTAlgorithm {
  given algorithm: CollaborativeTextEditingAlgorithm[OTAlgorithm] with {

    extension (algorithm: OTAlgorithm) {
      override def delete(i: Int): Unit = {
        if (algorithm.causalBroadcast.needsTick) {
          algorithm.causalBroadcast.needsTick = false
          algorithm.causalBroadcast.tick()
        }
        
        val message = OTOperation(algorithm.replicaId, OperationType.Delete(i), algorithm.causalBroadcast.causalState.clone())

        algorithm.causalBroadcast.addOneToHistory(
          message
        )

        algorithm.operationsPerSite.getOrElseUpdate(algorithm.replicaId, mutable.ArrayBuffer()).addOne(message)

        //println(s"produced message $message at ${algorithm.replicaId}")

        text.deleteCharAt(i)
      }

      override def insert(i: Int, x: Char): Unit = {
        if (algorithm.causalBroadcast.needsTick) {
          algorithm.causalBroadcast.needsTick = false
          algorithm.causalBroadcast.tick()
        }
        
        val message = OTOperation(algorithm.replicaId, OperationType.Insert(i, x), algorithm.causalBroadcast.causalState.clone())

        algorithm.causalBroadcast.addOneToHistory(
          message
        )

        algorithm.operationsPerSite.getOrElseUpdate(algorithm.replicaId, mutable.ArrayBuffer()).addOne(message)

        //println(s"produced message $message at ${algorithm.replicaId}")

        text.insert(i, x)
      }   

      override def text(): String = {
        text.toString()
      }

      override def sync(other: OTAlgorithm) = {
        algorithm.syncFrom(other)
        other.syncFrom(algorithm)
      }

      def getDifference(larger: CausalID, smaller: CausalID) = {
        //println(s"enter getDifference $larger $smaller")

        val returnValue = mutable.ArrayBuffer.from(larger.flatMap((key, value) => {
          val s = smaller.getOrElse(key, 0)

          if (s != value) {
            algorithm.operationsPerSite(key).slice(s, value)
          } else {
            mutable.ArrayBuffer()
          }
        }))
        //println(s"exit getDifference $sorted")
        returnValue
      }

      def isSorted[T](list: mutable.ArrayBuffer[T])(implicit ord: Ordering[T]): Boolean = list.headOption.fold(true)(a => list.tail.headOption.fold(true)(ord.lteq(a, _) && isSorted(list.tail.tail)))
      
      def cotTransform(operationParam: OTOperation, unsortedContextDifference: ArrayBuffer[OTOperation]): OTOperation = {
        val contextDifference = unsortedContextDifference.sortInPlaceBy(op => op.context.clone().addOne(op.replica, op.context.getOrElse(op.replica, 0)+1))(using CausalID.totalOrder)

        //println(s"enter cotTransform $operationParam $contextDifference")
        var operation = operationParam
        while (contextDifference.nonEmpty) {
          var operationX = contextDifference.remove(0)
          assert(CausalID.partialOrder.lteq(operationX.context, operation.context))
          operationX = cotTransform(operationX, getDifference(operation.context, operationX.context))
          operation = inclusionTransform(operation, operationX)

          // does inclusiontransform already do this?
          /*operation.context
            .update(
              operationX.replica,
              operation.context.getOrElse(operationX.replica, 0) + 1
            )*/
        }
        //println(s"exit cotTransform $operation")
        operation
      }

      def cotDo(operation: OTOperation, documentState: CausalID): Unit = {
        //println(s"enter cotDo $operation $documentState")
        assert(CausalID.partialOrder.lteq(operation.context, documentState))

        val transformedOperation = cotTransform(operation, getDifference(documentState, operation.context))

        assert(transformedOperation.context == documentState)

        //println(s"executing $transformedOperation at ${algorithm.replicaId}")

        transformedOperation.inner match {
          case OperationType.Identity => 
          case OperationType.Insert(i, x) => text.insert(i, x)
          case OperationType.Delete(i) => text.deleteCharAt(i)
        }
      }

      override def syncFrom(other: OTAlgorithm) = {
        algorithm.causalBroadcast.syncFrom(other.causalBroadcast, (otherCausalId, otherMessage) => {
          // important: the message must not be from the peer it was sent from
          algorithm.operationsPerSite.getOrElseUpdate(otherMessage.replica, mutable.ArrayBuffer()).addOne(otherMessage)

          //println(s"receiving $otherMessage with causal info ${otherCausalId} from ${other.replicaId} at ${algorithm.replicaId}")

          cotDo(otherMessage, algorithm.causalBroadcast.causalState)
        })
      }
    }
  }
}