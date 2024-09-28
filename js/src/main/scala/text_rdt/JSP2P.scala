package text_rdt

import org.scalajs.dom.Element
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLTextAreaElement
import org.scalajs.dom.RTCConfiguration
import org.scalajs.dom.RTCDataChannelInit
import org.scalajs.dom.RTCIceCandidate
import org.scalajs.dom.RTCIceCandidateInit
import org.scalajs.dom.RTCIceServer
import org.scalajs.dom.RTCPeerConnection
import org.scalajs.dom.RTCSdpType
import org.scalajs.dom.RTCSessionDescription
import org.scalajs.dom.RTCSessionDescriptionInit
import org.scalajs.dom.document
import typings.prosemirrorState.mod.EditorState
import upickle.default.*

import java.util.UUID
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

private case class JSP2P() {

  private var peers: Seq[RTCPeerConnection] = List()

  def awesomeFunc(shouldOffer: Boolean) = {

    val peer = document.createElement("div")
    peer.id = s"peer-${peers.size}"

    val child5 =
      document.createElement("textarea").asInstanceOf[HTMLTextAreaElement]
    child5.readOnly = true
    val _ = peer.appendChild(child5)

    val child6 =
      document.createElement("textarea").asInstanceOf[HTMLTextAreaElement]
    child6.placeholder = "Input received token"
    val _ = peer.appendChild(child6)

    val child7 = document.createElement("button")
    child7.innerText = "Connect using received token"
    val _ = peer.append(child7)

    val _ = org.scalajs.dom.document
      .querySelector("#peers")
      .appendChild(peer)

    val turnUrl = org.scalajs.dom.document
      .querySelector("#turn-url")
      .asInstanceOf[HTMLInputElement]
      .value;
    val turnUsername = org.scalajs.dom.document
      .querySelector("#turn-username")
      .asInstanceOf[HTMLInputElement]
      .value;
    val turnPassword = org.scalajs.dom.document
      .querySelector("#turn-password")
      .asInstanceOf[HTMLInputElement]
      .value;
    val turnServer = new RTCIceServer {
      urls = turnUrl;
      username = turnUsername;
      credential = turnPassword;
    }

    val stunServer = new RTCIceServer {
      urls = "stun:stun.l.google.com:19302"
    };

    val theIceServers = if (turnPassword.isEmpty()) {
      js.Array(stunServer)
    } else {
      js.Array(stunServer, turnServer)
    }

    org.scalajs.dom.console.log(theIceServers)

    val connection = RTCPeerConnection(
      new RTCConfiguration {
        iceServers = theIceServers;
      }
    )

    val channel = connection
      .createDataChannel(
        "test",
        new RTCDataChannelInit { id = 0; negotiated = true }
      )

    channel.addEventListener(
      "open",
      event => {

        var interval = 0;

        interval = org.scalajs.dom.window.setInterval(
          () => {
            if (channel.readyState == "open") {
              channel.send("heads" + write(replica.state.causalBroadcast.cachedHeads))
              replica.state.causalBroadcast.causalState = replica.state.causalBroadcast.causalState.clone()
              replica.state.causalBroadcast.tick()
            } else {
              org.scalajs.dom.window.clearInterval(interval)
            }
          },
          1000
        )

        channel.onmessage = event => {
          if (event.data.toString().startsWith("heads")) {
            val heads: ArrayBuffer[mutable.HashMap[String, Integer]] =
              read(event.data.toString().stripPrefix("heads"))
            val potentiallyNewer2 =
              replica.state.causalBroadcast
                .elementsPotentiallyNewer(
                  heads
                )
                .toSeq
            val result = write(potentiallyNewer2)
            replica.state.causalBroadcast.causalState = replica.state.causalBroadcast.causalState.clone()
            replica.state.causalBroadcast.tick()
            channel.send("diff" + result)
          } else if (event.data.toString().startsWith("diff")) {
            val potentiallyNewer: Seq[
              (
                  text_rdt.CausalID,
                  scala.collection.mutable.ArrayBuffer[
                    replica.state.factoryContext.MSG
                  ]
              )
            ] = read(event.data.toString().stripPrefix("diff"))
            potentiallyNewer.foreach(received =>
              replica.deliveringRemote(received)
            )
          }
        }
      }
    )

    connection.addEventListener(
      "connectionstatechange",
      event => {
        child5.value = connection
          .asInstanceOf[typings.std.global.RTCPeerConnection]
          .connectionState
          .toString()
      }
    )

    var sessionDescription: RTCSessionDescription | Null = null

    var candidates = Array[RTCIceCandidate]()

    connection.onicecandidate = event => {
      if (event.candidate eq null) {
        child5.value = write(
          (
            sessionDescription.nn.`type`.toString(),
            sessionDescription.nn.sdp,
            candidates.map(c => (c.candidate, c.sdpMid, c.sdpMLineIndex))
          )
        )
      } else {
        candidates = candidates.appended(event.candidate)
      }
    }

    child7.addEventListener(
      "click",
      event => {
        event.preventDefault()

        val input = child6.value

        val (
          incomingType: String,
          incomingSdp: String,
          iceCandidatesTuple: Array[
            (String, String, Double)
          ]
        ) = read[(String, String, Array[(String, String, Double)])](input)

        val iceCandidates = iceCandidatesTuple.map(c =>
          RTCIceCandidate(new RTCIceCandidateInit {
            candidate = c._1
            sdpMid = c._2
            sdpMLineIndex = c._3
          })
        )

        val incoming = RTCSessionDescription(new RTCSessionDescriptionInit {
          `type` = incomingType match {
            case "offer"  => RTCSdpType.offer
            case "answer" => RTCSdpType.answer
          };
          sdp = incomingSdp
        })

        incomingType match {
          case RTCSdpType.answer =>
            connection
              .setRemoteDescription(
                incoming
              )
              .toFuture
              .andThen(done => {
                child5.value = done.toString()
              })
          case RTCSdpType.offer =>
            connection
              .setRemoteDescription(incoming)
              .toFuture
              .andThen(done => {
                Future
                  .sequence(
                    iceCandidates
                      .map(connection.addIceCandidate(_).toFuture)
                  )
                  .andThen(iceResult => {
                    connection
                      .createAnswer()
                      .toFuture
                      .andThen(answerResult => {
                        answerResult match {
                          case Failure(exception) =>
                            exception.printStackTrace()
                            child5.value = exception.toString()
                          case Success(value) =>
                            sessionDescription = value
                            connection
                              .setLocalDescription(value)
                              .toFuture
                              .andThen(done => {})
                        }
                      })
                  })
              })
        }
      }
    )

    if (shouldOffer) {
      val offer = connection.createOffer().toFuture
      val _ = offer.andThen(offerResult => {
        offerResult match {
          case Failure(exception) =>
            exception.printStackTrace()
            child5.value = exception.toString()
          case Success(value) =>
            connection
              .setLocalDescription(value)
              .toFuture
              .andThen(done => {
                sessionDescription = value
              })
        }
      })
    }

    peers = peers.appended(connection)
  }

  org.scalajs.dom.document.body.innerHTML = """
  <input id="turn-url" type="url" value="turn:turn.selfmade4u.de:3478" placeholder="TURN url"></input>
  <input id="turn-username" type="text" value="moritz" placeholder="TURN username"></input>
  <input id="turn-password" type="password" placeholder="TURN password"></input>

  <button id="offer-connection">Offer connection</button>

  <button id="answer-connection">Answer connection</button>

  <div id="peers">

  </div>

  <div id="container" style="display: grid; grid-template-columns: 1fr 1fr;">
  </div>

  """

  val replicaId = UUID.randomUUID().nn.toString()

  val newEditor = document.createElement("div")
  newEditor.id = s"editor${replicaId}-parent"
  val child1 = document.createElement("h3")
  child1.innerText = s"Editor"
  val child2 = document.createElement("div")
  child2.classList.add("my-border")
  child2.id = s"editor${replicaId}"
  val child3 = document.createElement("pre")
  child3.id = s"editor${replicaId}-info"
  val child4 = document.createElement("button")
  child4.id = s"editor${replicaId}-toggle-graph"
  child4.innerText = "Toggle graphs"
  val child55 = document.createElement("div")
  child55.id = s"editor${replicaId}-graph"
  val child6 = document.createElement("div")
  child6.id = s"editor${replicaId}-graph-avl"
  val _ = newEditor.appendChild(child1)
  val _ = newEditor.appendChild(child2)
  val _ = newEditor.appendChild(child3)
  val _ = newEditor.appendChild(child4)
  val _ = newEditor.appendChild(child55)
  val _ = newEditor.appendChild(child6)

  val _ = org.scalajs.dom.document
    .querySelector("#container")
    .appendChild(newEditor)

  val replicaState =
    new ReplicaState[complexAVLTreeNodeD3Tree.factoryContext.type](
      replicaId
    )(using complexAVLTreeNodeD3Tree.factoryContext)

  val state = EditorState.create(
    editorStateConfig
  )

  val replica =
    Replica[complexAVLTreeNodeD3Tree.factoryContext.type](
      replicaState,
    )

  val prosemirror =
    ProseMirror(
      s"#editor${replicaId}",
      state,
      complexAVLTreeNodeD3Tree,
      replica
    )

  replica.editor = prosemirror

  org.scalajs.dom.document
    .querySelector("#offer-connection")
    .addEventListener(
      "click",
      event => {
        event.preventDefault()
        awesomeFunc(true)
      }
    )

  org.scalajs.dom.document
    .querySelector("#answer-connection")
    .addEventListener(
      "click",
      event => {
        event.preventDefault()
        awesomeFunc(false)
      }
    )
}
