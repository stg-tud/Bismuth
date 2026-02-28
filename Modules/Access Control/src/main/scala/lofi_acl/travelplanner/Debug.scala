package lofi_acl.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.PublicIdentity
import lofi_acl.bft.AclRdt.given_JsonValueCodec_BftDelta
import lofi_acl.bft.{Acl, BftDelta, Hash, Signature}
import lofi_acl.sync.signed.AclEnforcingSync.SyncMsg
import lofi_acl.sync.signed.AclEnforcingSync.SyncMsg.{AclDeltas, DataDeltas, MyAclVersionIs, MyPeersAre}
import lofi_acl.sync.signed.SignedDelta
import rdts.base.Uid
import rdts.time.{Dot, Dots}

object Debug {
  inline val enabled = true

  def shorten(signature: Signature): String = "✍️" + signature.toString.substring(0, 2)

  def shorten(hash: Hash): String = "#" + hash.toString.substring(0, 2)

  def shorten(id: PublicIdentity): String = "🪪" + id.id.substring(0, 2)

  def shorten(uid: Uid): String = "🪪" + uid.delegate.substring(0, 2)

  def shorten(acl: Acl): String = {
    "r=" + acl.read.map((id, perm) => s"${shorten(id)}->$perm").mkString("{", ",", "}") +
    "|w=" + acl.write.map((id, perm) => s"${shorten(id)}->$perm").mkString("{", ",", "}")
  }

  def shorten(aclDeltas: Seq[BftDelta[Acl]]): String =
    aclDeltas.map(shorten).mkString(" , ")

  def shorten(aclDelta: BftDelta[Acl]): String = aclDelta match {
    case BftDelta(sig, author, state, parents) =>
      val hash = shorten(Hash.compute(writeToArray(aclDelta)))
      s"$hash->{${shorten(author)}|${shorten(state)}|${parents.map(shorten)}}"
  }

  def shorten(dot: Dot): String = s"${dot.place.toString.substring(0, 4)}@${dot.time}"

  def shorten(dots: Dots): String =
    s"[${dots.internal.map { (uid, arrayRanges) => shorten(uid) + "@" + arrayRanges }.mkString(",")}]"

  def shorten(signedDelta: SignedDelta[?]): String = signedDelta match {
    case SignedDelta(dot, signature, payload) =>
      val payloadString = payload match {
        case tp: TravelPlan => shorten(tp)
        case _              => payload.toString
      }
      s"${shorten(dot)}->$payloadString"
  }

  def shorten(travelPlan: TravelPlan): String =
    // writeToString(travelPlan)
    travelPlan.toString
      .replaceAll("🪪(..).*=", "🪪$1")
      .replaceAll("LastWriterWins", "LWW")
      .replaceAll("ObserveRemoveMap", "ORMap")
      .replaceAll("CausalTime", "")


  def shorten(syncMsg: SyncMsg[?]): String = {
    syncMsg match {
      case AclDeltas(delta) =>
        s"AclDeltas(${shorten(delta)})".replaceAll("PermissionTree","")
      case MyPeersAre(peers) =>
        peers.map((id, addr) => s"${shorten(id)}->$addr").mkString("MyPeersAre(", ",", ")")
      case MyAclVersionIs(aclHeads) =>
        aclHeads.map(shorten).mkString("MyAclVersionIs(", ",", ")")
      case DataDeltas(deltas, filtered, acl) =>
        s"DataDeltas([${deltas.map(shorten).mkString(",")}], ${shorten(filtered)}, ${acl.map(shorten)})"
      case _ => syncMsg.toString
    }
  }

  inline def log(inline msg: => String): Unit =
    inline if enabled then println(msg)

  inline def received(msg: SyncMsg[?], from: PublicIdentity): Unit = {
    inline if enabled
    then
        println(s"${shorten(from)} ? ${shorten(msg)}")
  }

  inline def sent(msg: SyncMsg[?], to: PublicIdentity): Unit = {
    inline if enabled
    then
        println(s"${shorten(to)} ! ${shorten(msg)}")
  }
}
