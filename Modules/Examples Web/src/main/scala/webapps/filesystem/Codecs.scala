package webapps.filesystem

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import rdts.datatypes.ReplicatedTree
import rdts.time.{Dot, Dots}
import rdts.base.Uid
import rdts.experiments.UndoRedoReplica

object Codecs {
  given codecRGA: JsonValueCodec[UndoRedoReplica[FilesystemState]] =
    JsonCodecMaker.make[UndoRedoReplica[FilesystemState]](
      CodecMakerConfig.withMapAsArray(true)
    )
}
