package de.tu_darmstadt.informatik.st.reform

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object BasicCodecs {

  // every client has an id
  val myReplicaID: rdts.base.LocalUid = rdts.base.LocalUid.gen()

  implicit val stringCodec: JsonValueCodec[String] = JsonCodecMaker.make

  implicit val idCodec: JsonValueCodec[rdts.base.Uid] = new JsonValueCodec[rdts.base.Uid] {
    private val codec = JsonCodecMaker.make[String]

    override def decodeValue(
        in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader,
        default: rdts.base.Uid
    ): rdts.base.Uid =
      rdts.base.Uid.predefined(codec.decodeValue(in, ""))

    override def encodeValue(x: rdts.base.Uid, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
      codec.encodeValue(rdts.base.Uid.unwrap(x), out)

    override def nullValue: rdts.base.Uid = rdts.base.Uid.zero
  }
}
