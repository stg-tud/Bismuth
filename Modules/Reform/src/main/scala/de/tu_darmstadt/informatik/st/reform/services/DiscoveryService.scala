/*
Copyright 2022 The reform-org/reform contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--- NOTE: scala-loci peer-to-peer sync has been removed. The always-online
--- peer WebSocket connection (loci.communicator.ws.webnative.WS) has been
--- removed. The discovery server WebSocket (for peer discovery and WebRTC
--- signalling) still works as before.
 */
package de.tu_darmstadt.informatik.st.reform.services

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.tu_darmstadt.informatik.st.reform.JSImplicits
import de.tu_darmstadt.informatik.st.reform.*
import de.tu_darmstadt.informatik.st.reform.given_ExecutionContext
import de.tu_darmstadt.informatik.st.reform.utils.Cookies
import de.tu_darmstadt.informatik.st.reform.utils.Futures.*
import de.tu_darmstadt.informatik.st.reform.webrtc.PendingConnection
import org.scalajs.dom
import org.scalajs.dom.*
import reactives.default.*

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.Date
import scala.scalajs.js.JSON
import scala.util.Failure

class AvailableConnection(
    val name: String,
    val uuid: String,
    val displayId: String,
    val trusted: Boolean,
    val mutualTrust: Boolean,
    val tpe: String,
)

class LoginException(val message: String, val fields: Seq[String]) extends Throwable(message)

class LoginInfo(val username: String, val password: String)
object LoginInfo {
  val codec: JsonValueCodec[LoginInfo] = JsonCodecMaker.make
}

class DiscoveryService {
  private var pendingConnections: Map[String, PendingConnection] = Map()
  private var ws: Option[WebSocket]                              = None

  class LoginRepsonse(val token: String, val username: String)
  object LoginRepsonse {
    val codec: JsonValueCodec[LoginRepsonse] = JsonCodecMaker.make
  }

  class TokenPayload(val exp: Int, val iat: Int, val username: String, val uuid: String, val tpe: String)

  val availableConnections: Var[Seq[AvailableConnection]] = Var(Seq.empty)

  val token: Var[Option[String]] = Var(Cookies.getCookie("discovery-token"))

  val online: Var[Boolean] = Var(false)

  def setAutoconnect(using
      jsImplicits: JSImplicits
  )(
      value: Boolean,
  ): Unit = {
    window.localStorage.setItem("autoconnect", value.toString)
    autoconnect.set(value)
    if value then {
      connect()
        .toastOnError()
    } else {
      close()
    }
  }

  def decodeToken(token: String): TokenPayload = {
    val decodedToken = JSON.parse(window.atob(token.split('.')(1)))
    TokenPayload(
      decodedToken.exp.asInstanceOf[Int],
      decodedToken.iat.asInstanceOf[Int],
      decodedToken.username.asInstanceOf[String],
      decodedToken.uuid.asInstanceOf[String],
      decodedToken.`type`.asInstanceOf[String],
    )
  }

  private def updateToken(value: Option[String]): Unit = {
    value match {
      case Some(value) =>
        Cookies.setCookie("discovery-token", value)
      case None =>
        Cookies.clearCookie("discovery-token")
    }
    token.set(value)
  }

  def tokenIsValid(token: Option[String]): Boolean =
    token.nonEmpty && !token.get.isBlank && Date.now() > decodeToken(token.get).exp

  def logout(): Unit = {
    ws match {
      case None         =>
      case Some(socket) => ws.get.close()
    }
    updateToken(None)
  }

  def login(using
      jsImplicits: JSImplicits
  )(
      loginInfo: LoginInfo,
  ): Future[String] = {
    val promise = Promise[String]()

    if !tokenIsValid(token.now) then {
      println("[DiscoveryService] login disabled (scala-loci removed)")
      promise.failure(new Exception("Discovery server login disabled"))
    }

    promise.future
  }

  def disconnect(ref: String): Unit = {
    println(s"[DiscoveryService] disconnect('$ref') is a no-op (scala-loci removed)")
  }

  def addToWhitelist(uuid: String): Unit =
    ws.foreach(emit(_, "whitelist_add", js.Dynamic.literal("uuid" -> uuid)))

  def connectTo(uuid: String): Unit =
    ws.foreach(emit(_, "connect_to", js.Dynamic.literal("uuid" -> uuid)))

  def reportClosedConnection(id: String): Unit =
    ws.foreach(emit(_, "connection_closed", js.Dynamic.literal("connection" -> id)))

  def deleteFromWhitelist(uuid: String): Unit =
    ws.foreach(emit(_, "whitelist_del", js.Dynamic.literal("uuid" -> uuid)))

  def refetchAvailableClients(): Unit =
    ws.foreach(emit(_, "request_available_clients", null))

  private def emit(ws: WebSocket, name: String, payload: js.Any | Null): Unit = {
    val event = js.Dynamic.literal("type" -> name, "payload" -> payload.asInstanceOf[js.Any])
    ws.send(JSON.stringify(event))
  }

  private def getRTCIceServers(payload: js.Dynamic): RTCConfiguration = {
    // https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer/urls
    new RTCConfiguration {
      iceServers = js.Array(
        new RTCIceServer {
          urls = s"stun:${Globals.VITE_TURN_SERVER_HOST}:${Globals.VITE_TURN_SERVER_PORT}"
        },
        new RTCIceServer {
          urls = s"turn:${Globals.VITE_TURN_SERVER_HOST}:${Globals.VITE_TURN_SERVER_PORT}"
          username = payload.client.turnKey.username.asInstanceOf[String]
          credential = payload.client.turnKey.credential.asInstanceOf[String]
        },
      )
    }
  }

  private def handle(using jsImplicits: JSImplicits)(ws: WebSocket, name: String, payload: js.Dynamic) = {
    if name != "ping" then console.log(name, payload)

    name match {
      case "request_host_token" =>
        pendingConnections += (payload.id.asInstanceOf[String] -> PendingConnection.webrtcIntermediate(
          de.tu_darmstadt.informatik.st.reform.webrtc.WebRTC.offer(getRTCIceServers(payload)),
          payload.client.user.name.asInstanceOf[String],
        ))

        pendingConnections(payload.id.asInstanceOf[String]).session
          .map(PendingConnection.sessionAsToken)
          .foreach { token =>
            emit(ws, "host_token", js.Dynamic.literal("token" -> token, "connection" -> payload.id))
          }

        jsImplicits.webrtc.registerConnection(
          payload.client.user.name.asInstanceOf[String],
        )
      case "request_client_token" =>
        pendingConnections += (payload.id.asInstanceOf[String] -> PendingConnection.webrtcIntermediate(
          de.tu_darmstadt.informatik.st.reform.webrtc.WebRTC.answer(getRTCIceServers(payload)),
          payload.host.user.name.asInstanceOf[String],
        ))

        pendingConnections(payload.id.asInstanceOf[String]).session
          .map(PendingConnection.sessionAsToken)
          .foreach { token =>
            emit(ws, "client_token", js.Dynamic.literal("token" -> token, "connection" -> payload.id))
          }

        jsImplicits.webrtc.registerConnection(
          payload.host.user.name.asInstanceOf[String],
        )
      case "available_clients" =>
        val clients = payload.clients
          .asInstanceOf[js.Array[js.Dynamic]]
          .map(client =>
            new AvailableConnection(
              client.name.asInstanceOf[String],
              client.id.asInstanceOf[String],
              client.displayId.asInstanceOf[String],
              client.trusted.asInstanceOf[Int] != 0,
              client.mutualTrust.asInstanceOf[Int] != 0,
              client.`type`.asInstanceOf[String],
            ),
          )
        val clientsSeq: Seq[AvailableConnection] = clients.toSeq
        availableConnections.set(clientsSeq)
      case "request_client_finish_connection" =>
        pendingConnections -= payload.id.asInstanceOf[String]
        emit(ws, "finish_connection", js.Dynamic.literal("connection" -> payload.id))
      case "request_host_finish_connection" =>
        pendingConnections -= payload.id.asInstanceOf[String]
        emit(ws, "finish_connection", js.Dynamic.literal("connection" -> payload.id))
      case "ping" =>
        emit(ws, "pong", null)
      case "connection_closed" =>
        println(s"[DiscoveryService] connection_closed: ${payload.id} (scala-loci removed)")
    }
  }

  def close(): Unit = {
    ws.foreach(ws => ws.close())
    ws = None
  }

  def connect(using
      jsImplicits: JSImplicits,
  )(resetWebsocket: Boolean = false, force: Boolean = false): Future[Boolean] = {
    val promise = Promise[Boolean]()

    if resetWebsocket then ws = None

    if Option(window.localStorage.getItem("autoconnect")).getOrElse("true").toBoolean || force then {
      if !tokenIsValid(token.now) then {
        return promise.failure(new Exception("Your token is wrong")).future
      }

      println(s"[DiscoveryService] would connect to discovery server websocket, but scala-loci has been removed")
      promise.success(true)

      // NOTE: always-online peer connection via scala-loci WS communicator has been removed
      println("[DiscoveryService] always-online peer connection disabled (scala-loci removed)")

      promise.future
    } else {
      Future.successful(true)
    }
  }
}
