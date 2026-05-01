package replication.research

import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */
object WebRtcSignalingProtocol {

  case class Session(descType: String, sdp: String)

  enum Message {
    case Register(uid: Uid)
    case Offer(from: Uid, to: Uid, session: Session)
    case Answer(from: Uid, to: Uid, session: Session)
  }
}
