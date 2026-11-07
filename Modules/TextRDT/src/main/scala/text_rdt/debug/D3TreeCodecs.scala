package text_rdt
package debug

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

given myD3TreeNodeCodec: JsonValueCodec[MyD3TreeNode] =
  JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
