package text_rdt.fugue.complexavl

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import text_rdt.fugue.{*, given}
import text_rdt.{*, given}

given complexAVLMessageCodec: JsonValueCodec[ComplexAVLMessage] = JsonCodecMaker.make
