package benchmarks.chatserver

import scala.collection.LinearSeq
import scala.collection.immutable.Queue

class ChatServer[Api <: reactives.default.type]()(val engine: Api) {

  import engine.*

  type Room        = Int
  type Client      = Event[String]
  type Clients     = Var[List[Client]]
  type NewMessages = Event[List[String]]
  type History     = Signal[LinearSeq[String]]

  val rooms     = new java.util.concurrent.ConcurrentHashMap[Room, Clients]()
  val histories = new java.util.concurrent.ConcurrentHashMap[Room, History]()

  def join(client: Client, room: Room) = {
    rooms.get(room).transform(clients => client :: clients)
  }

  def create(room: Room): Boolean = {
    val clients: Clients         = Var(Nil)
    val newMessages: NewMessages = Event.dynamic {
      val messages: List[String] = clients.value.flatMap(_.value)
      if messages.isEmpty then None else Some(messages)
    }
    val history: History = newMessages.fold(Queue[String]()) { (queue, v) =>
      if queue.length >= 100 then queue.tail.enqueueAll(v) else queue.enqueueAll(v)
    }

    rooms.putIfAbsent(room, clients) == null &&
    histories.put(room, history) == null
  }

}
