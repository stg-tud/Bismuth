package probench.data

import probench.data.RequestResponseQueue.{Req, Res, Timestamp}
import rdts.base.*
import rdts.base.Lattice.mapLattice
import rdts.base.LocalUid.replicaId
import rdts.datatypes.ObserveRemoveMap
import rdts.time.CausalTime

case class RequestResponseQueue[S,T](
    requests: ObserveRemoveMap[Timestamp, Req[S]] = ObserveRemoveMap.empty[Timestamp, Req[S]],
    responses: ObserveRemoveMap[Timestamp, Res[T]] = ObserveRemoveMap.empty[Timestamp, Res[T]]
) {
  def request(value: S)(using LocalUid): (Timestamp, RequestResponseQueue[S,T]) =
      // find the newest timestamp
      val timestamp = timestampsSorted.lastOption match
          case Some((time, _)) => (time.advance, replicaId)
          case None            => (CausalTime.now(), replicaId)

      (timestamp, RequestResponseQueue(requests = requests.update(timestamp, Req(value, replicaId, timestamp))))

  def respond(request: Req[S], value: T)(using LocalUid): RequestResponseQueue[S,T] =
    RequestResponseQueue(
      requests = requests.remove(request.timestamp),
      responses =
        responses.update(request.timestamp, Res(value))
    )

  def responseTo(req: Req[S]): Option[Res[T]] =
    responses.get(req.timestamp)

  /**
   * receive the response with given
   */
  def receive(timestamp: Timestamp): RequestResponseQueue[S,T] =
    RequestResponseQueue(responses = responses.remove(timestamp))

  def firstUnansweredRequest: Option[Req[S]] = {
    timestampsSorted.headOption.flatMap(requests.get)
  }

  private def timestampsSorted: List[Timestamp] =
    requests.keySet.toList.sorted
}

object RequestResponseQueue {
  type Timestamp = (CausalTime, Uid)

  case class Req[+T](value: T, requester: Uid, timestamp: Timestamp)
  case class Res[+T](value: T)

  def empty[S,T]: RequestResponseQueue[S,T] = RequestResponseQueue()

  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S,T]] = Bottom.provide(empty)

  given Ordering[Timestamp] = Orderings.lexicographic

  // lattices
  given [S,T]: Lattice[RequestResponseQueue[S,T]] =
      given Lattice[Req[S]] = Lattice.assertEquals
      given Lattice[Res[T]] = Lattice.assertEquals
      Lattice.derived
}
