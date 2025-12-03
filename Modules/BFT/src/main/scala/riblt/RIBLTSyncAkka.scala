package riblt

import akka.actor.typed.*
import akka.actor.typed.scaladsl.*
import datatypes.Replica
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String, given_JsonValueCodec_CodedSymbol}
import riblt.RIBLTSyncAkka.SessionType.sender

object RIBLTSyncAkka {

  sealed trait Command
  final case class StartSession(otherReplica: ActorRef[Command], sessionType: SessionType)            extends Command
  final case class ReceiveCodedSymbols(from: ActorRef[Command], codedSymbols: List[Array[Byte]])      extends Command
  final case class ReceiveDelta[T, R <: Replica[T, R]](from: ActorRef[Command], delta: Replica[T, R]) extends Command
  final case class ReceiveCodedSymbolsRequest(from: ActorRef[Command])                                extends Command
  final case class ReceiveDeltaRequest(from: ActorRef[Command], ids: List[String])                    extends Command
  final case class GetReplica[T, R <: Replica[T, R]](replyTo: akka.actor.typed.ActorRef[ReplicaResponse])
      extends Command
  final case class ReplicaResponse(replica: Any) extends Command

  final case class Session(
      sessionType: SessionType,
      riblt: RIBLT[String],
      isDecoded: Boolean = false,
      isSynced: Boolean = false,
      deltaReceived: Boolean = false,
      deltaSent: Boolean = false
  )

  enum SessionType:
      case sender, receiver

  def apply[T, R <: Replica[T, R]](replicaID: String, initialReplica: R): Behavior[Command] = {
    Behaviors.setup { context =>
      def running(replica: R, sessions: Map[ActorRef[Command], Session]): Behavior[Command] = {
        Behaviors.receiveMessage {

          // Start new session
          case StartSession(other, sessionType) =>
            val riblt = RIBLT[String]()
            for id <- replica.hashDAG.getIDs do riblt.addSymbol(id)
            val newSession = Session(sessionType, riblt)
            // println(s"[$replicaID] started session with ${other.path.name} as $sessionType")

            if sessionType == sender then {
              val symbols = riblt.produceNextCodedSymbolsAsBytes()
              other ! ReceiveCodedSymbols(context.self, symbols)
            }

            running(replica, sessions.updated(other, newSession))

          // Handle coded symbols
          case ReceiveCodedSymbols(from, codedSymbols) =>
            // println(s"[$replicaID] received coded symbols from ${from.path.name}")
            val session = sessions(from)
            session.riblt.addCodedSymbolsAsBytes(codedSymbols)
            if session.riblt.isDecoded then
                val delta = replica.generateDelta(session.riblt.localSymbols.map(_.value))
                from ! ReceiveDelta(context.self, delta)
                from ! ReceiveDeltaRequest(context.self, session.riblt.remoteSymbols.map(_.value))
                val updated = session.copy(isDecoded = true, deltaSent = true)
                running(replica, sessions.updated(from, updated))
            else
                from ! ReceiveCodedSymbolsRequest(context.self)
                Behaviors.same

          // Handle coded symbols request
          case ReceiveCodedSymbolsRequest(from) =>
            // println(s"[$replicaID] received coded symbol request from ${from.path.name}")
            val session = sessions(from)
            val symbols = session.riblt.produceNextCodedSymbolsAsBytes()
            from ! ReceiveCodedSymbols(context.self, symbols)
            Behaviors.same

          // Handle delta
          case ReceiveDelta(from, deltaAny) =>
            val delta = deltaAny.asInstanceOf[R]
            // println(s"[$replicaID] received delta from ${from.path.name}")
            val newReplica = replica.merge(delta)
            // println("**************************************************")
            // println(newReplica.hashDAG.events.map(e => e._2.content))
            // println("**************************************************")

            val updatedSession = sessions(from).copy(deltaReceived = true)
            val stillActive    = !updatedSession.deltaSent || !updatedSession.deltaReceived
            val newSessions    =
              if stillActive then sessions.updated(from, updatedSession)
              else sessions - from
            running(newReplica, newSessions)

          // Handle delta request
          case ReceiveDeltaRequest(from, ids) =>
            // println(s"[$replicaID] received delta request from ${from.path.name}")
            val delta = replica.generateDelta(ids)
            from ! ReceiveDelta(context.self, delta)
            val updatedSession = sessions(from).copy(deltaSent = true)
            val stillActive    = !updatedSession.deltaReceived || !updatedSession.deltaSent
            val newSessions    =
              if stillActive then sessions.updated(from, updatedSession)
              else sessions - from
            running(replica, newSessions)

          case RIBLTSyncAkka.GetReplica(replyTo) =>
            // println("///////////////////////////////////")
            // println(replica.hashDAG.events.map(e => e._2.content))
            // println("////////////////////////////////////")
            replyTo ! RIBLTSyncAkka.ReplicaResponse(replica)
            Behaviors.same

          case _ => Behaviors.same
        }
      }

      running(initialReplica, Map.empty)
    }
  }
}
