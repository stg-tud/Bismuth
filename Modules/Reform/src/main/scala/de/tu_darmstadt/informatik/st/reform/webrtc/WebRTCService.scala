/*
Copyright 2022 https://github.com/phisn/ratable, The reform-org/reform contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--- NOTE: scala-loci peer-to-peer sync has been removed. WebRTC service,
--- BroadcastChannel, and all remote peer connections have been removed.
 */
package de.tu_darmstadt.informatik.st.reform.webrtc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.tu_darmstadt.informatik.st.reform.utils.Base64
import org.scalajs.dom.RTCPeerConnection
import reactives.default.*

import scala.concurrent.Future
import scala.concurrent.Promise

class ConnectionInformation(val session: WebRTCSession, val alias: String, val source: String = "manual") {}
class StoredConnectionInformation(
    var alias: String,
    val source: String = "manual",
    val uuid: String = "",
    val displayId: String = "",
    val connectionId: String = "",
    val tpe: String = "CLASSIC",
) {} // different object for discovery and manual

// Minimal stand-in for scala-loci's WebRTC session types
case class WebRTCSession(sdp: String, `type`: String)
case class PendingConnection(
    connector: ConnectorHandle,
    session: Future[ConnectionInformation],
    connection: RTCPeerConnection,
)
object PendingConnection {
  // Stub – scala-loci removed, WebRTC connections no longer work
  def webrtcIntermediate(cf: ConnectorHandle, alias: String): PendingConnection = {
    println(s"[PendingConnection] scala-loci removed – WebRTC connection to '$alias' will not proceed")
    val p      = Promise[ConnectionInformation]()
    val handle = ConnectorHandle()
    PendingConnection(handle, p.future, null)
  }
  private val codec: JsonValueCodec[ConnectionInformation] = JsonCodecMaker.make
  def sessionAsToken(s: ConnectionInformation): String     = Base64.encode(writeToString(s)(using codec))

  def tokenAsSession(s: String): ConnectionInformation = readFromString(Base64.decode(s))(using codec)
}

// Stub for scala-loci's Connector/ConnectorFactory
class ConnectorHandle() {
  def set(session: WebRTCSession): Unit =
    println("[ConnectorHandle] scala-loci removed – session set is a no-op")
}
object ConnectorHandle {
  def offer(config: Any): ConnectorHandle = {
    println("[ConnectorHandle] scala-loci removed – WebRTC offer not available")
    new ConnectorHandle()
  }
  def answer(config: Any): ConnectorHandle = {
    println("[ConnectorHandle] scala-loci removed – WebRTC answer not available")
    new ConnectorHandle()
  }
}

// Stub for scala-loci's WebRTC
object WebRTC {
  def offer(config: Any): ConnectorHandle  = ConnectorHandle.offer(config)
  def answer(config: Any): ConnectorHandle = ConnectorHandle.answer(config)
}

class WebRTCService() {

  println("[WebRTCService] scala-loci sync removed – peer connections disabled")

  private var connectionInfo = Map[String, StoredConnectionInformation]()

  private val removeConnection  = Evt[String]()
  private val addConnection     = Evt[String]()
  private val addConnectionB    = addConnection.branch(v => current[Seq[String]] :+ v)
  private val removeConnectionB = removeConnection.branch(r => current[Seq[String]].filter(b => !b.equals(r)))

  // Changed from RemoteRef to String (connection id / alias) since loci is removed
  val connections: Signal[Seq[String]] = Fold(Seq.empty: Seq[String])(addConnectionB, removeConnectionB)

  def registerConnection(alias: String): Future[Unit] = {
    println(s"[WebRTCService] scala-loci removed – registration of '$alias' is a no-op")
    Future.successful(())
  }

  def closeConnectionById(id: String): Unit =
    println(s"[WebRTCService] closeConnectionById('$id') is a no-op (scala-loci removed)")

  def getInformation(ref: String): StoredConnectionInformation =
    connectionInfo.getOrElse(ref, StoredConnectionInformation("Anonymous", "unknown"))

  def setAlias(ref: String, alias: String): Unit = {
    connectionInfo = connectionInfo.transform { (r, storedConnection) =>
      if ref == r then {
        storedConnection.alias = alias
      }
      storedConnection
    }
  }

  def getConnectionMode(ref: String): Future[String] =
    Future.successful("none (scala-loci removed)")
}
