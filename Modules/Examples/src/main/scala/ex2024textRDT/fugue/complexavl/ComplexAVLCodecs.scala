package ex2024textRDT.fugue.complexavl

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import ex2024textRDT.fugue.{*}
import ex2024textRDT.{*, given}

given complexAVLMessageCodec: JsonValueCodec[ComplexAVLMessage] = JsonCodecMaker.make
