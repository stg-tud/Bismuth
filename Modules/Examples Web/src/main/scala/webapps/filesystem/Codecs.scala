package webapps.filesystem

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.syntax.DeltaBuffer
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import rdts.datatypes.ReplicatedTree
import rdts.time.{Dot, Dots}
import rdts.base.Uid

object Codecs {
  given codecRGA: JsonValueCodec[DeltaBuffer[FilesystemState]] =
    JsonCodecMaker.make[DeltaBuffer[FilesystemState]](
      CodecMakerConfig.withMapAsArray(true)
    )
}
