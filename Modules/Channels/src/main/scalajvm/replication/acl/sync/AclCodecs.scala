package replication.acl.sync

import channels.JsoniterCodecs.given
import channels.JsoniterCodecsJvm.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.acl.sync.AclEnforcingSync.SyncMsg

object AclCodecs {
  given filterableSignedDeltaCodec[State: JsonValueCodec]: JsonValueCodec[SignedDelta[State]] =
    JsonCodecMaker.make

  given syncMsgCodec[State: JsonValueCodec]: JsonValueCodec[SyncMsg[State]] =
    JsonCodecMaker.make
}
