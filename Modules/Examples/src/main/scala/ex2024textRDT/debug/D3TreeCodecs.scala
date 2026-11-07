package ex2024textRDT.debug

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import ex2024textRDT.{*}

given myD3TreeNodeCodec: JsonValueCodec[MyD3TreeNode] =
  JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
