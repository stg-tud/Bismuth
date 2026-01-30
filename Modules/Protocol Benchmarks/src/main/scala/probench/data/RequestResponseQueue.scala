package probench.data

import probench.data.RequestResponseQueue.{Req, Res, Timestamp, given}
import rdts.base.*
import rdts.base.LocalUid.replicaId
import rdts.datatypes.ObserveRemoveMap
import rdts.datatypes.ObserveRemoveMap.lattice
import rdts.time.CausalTime

case class RequestResponseQueue[S, T](
    requests: ObserveRemoveMap[Timestamp, Req[S]] = ObserveRemoveMap.empty[Timestamp, Req[S]],
    responses: ObserveRemoveMap[Timestamp, Res[T]] = ObserveRemoveMap.empty[Timestamp, Res[T]]
) {
  def request(value: S)(using LocalUid): (Timestamp, RequestResponseQueue[S, T]) =
      // find the newest timestamp
      val timestamp = timestampsSorted.lastOption match
          case Some((time, _)) => (time.advance, replicaId)
          case None            => (CausalTime.now(), replicaId)

      // when client adds new request, it is assumed that the previous responses were processed
      val myResponses = responses.keySet.filter(t => t._2 == replicaId)

      (
        timestamp,
        RequestResponseQueue(
          requests = requests.update(timestamp, Req(value, timestamp)),
          responses = responses.removeAll(myResponses)
        )
      )

  def respond(request: Req[S], value: T)(using LocalUid): RequestResponseQueue[S, T] = {
    val previousResonses = responses.keySet.filter(t => t._2 == request.timestamp._2)
    val clearedResponses: ObserveRemoveMap[Timestamp, Res[T]] = responses.removeAll(previousResonses)
    val newResponse: ObserveRemoveMap[Timestamp, Res[T]]      = responses.update(request.timestamp, Res(value))
    RequestResponseQueue(
      requests = requests.remove(request.timestamp),
      responses =
        ObserveRemoveMap.lattice[Timestamp, Res[T]].merge(newResponse, clearedResponses)
    )
  }

  def responseTo(req: Req[S]): Option[Res[T]] =
    responses.get(req.timestamp)

  /** receive the response with given */
  def receive(timestamp: Timestamp): RequestResponseQueue[S, T] =
    RequestResponseQueue(responses = responses.remove(timestamp))

  def firstUnansweredRequest: Option[Req[S]] =
    timestampsSorted.headOption.flatMap(requests.get)

  def sortedUnansweredRequests: Seq[Req[S]] = timestampsSorted.flatMap(requests.get)

  private def timestampsSorted: List[Timestamp] =
    requests.keySet.toList.sorted

  def requestsSorted: List[Req[S]] =
    requests.entries.toList.sortBy(_._1).map(_._2)
}

object RequestResponseQueue {
  type Timestamp = (CausalTime, Uid)

  case class Req[+T](value: T, timestamp: Timestamp)
  case class Res[+T](value: T)

  def empty[S, T]: RequestResponseQueue[S, T] = RequestResponseQueue()

  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S, T]] = Bottom.provide(empty)

  given Ordering[Timestamp] = Orderings.lexicographic

  given [T]: Lattice[Res[T]] = Lattice.assertEquals

  // lattices
  given [S, T]: Lattice[RequestResponseQueue[S, T]] =
      given Lattice[Req[S]] = Lattice.assertEquals
      Lattice.derived
}
