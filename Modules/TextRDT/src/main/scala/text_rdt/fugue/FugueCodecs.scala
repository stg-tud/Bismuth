package text_rdt.fugue

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import text_rdt.{*, given}

given simpleIDCodec: JsonValueCodec[SimpleID]                  = JsonCodecMaker.make
given nullableSimpleIDCodec: JsonValueCodec[SimpleID | Null]   = nullableCodec[SimpleID]
given complexIDCodec: JsonValueCodec[ComplexID]                = JsonCodecMaker.make
given nullableComplexIDCodec: JsonValueCodec[ComplexID | Null] = nullableCodec[ComplexID]

given simpleMessageCodec: JsonValueCodec[Message[SimpleID]]   = JsonCodecMaker.make
given complexMessageCodec: JsonValueCodec[Message[ComplexID]] = JsonCodecMaker.make
