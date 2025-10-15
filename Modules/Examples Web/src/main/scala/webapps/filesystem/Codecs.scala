package webapps.filesystem

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.syntax.DeltaBuffer
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

object Codecs {
  given codecRGA: JsonValueCodec[DeltaBuffer[FilesystemState]] =
    JsonCodecMaker.make[DeltaBuffer[FilesystemState]](
      CodecMakerConfig.withMapAsArray(true)
    )
}
