package calendar

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.datatypes.ReplicatedSet
import rdts.syntax.DeltaBuffer

object Codecs {

  given codecRGA: JsonValueCodec[DeltaBuffer[ReplicatedSet[Appointment]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

}
