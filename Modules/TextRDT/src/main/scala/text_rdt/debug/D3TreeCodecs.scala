package text_rdt.debug

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import text_rdt.{*, given}

given myD3TreeNodeCodec: JsonValueCodec[MyD3TreeNode] =
  JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
