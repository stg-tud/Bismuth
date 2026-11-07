package ex2024textRDT.fugue

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import ex2024textRDT.{*, given}

given simpleIDCodec: JsonValueCodec[SimpleID]                  = JsonCodecMaker.make
given nullableSimpleIDCodec: JsonValueCodec[SimpleID | Null]   = nullableCodec[SimpleID]
given complexIDCodec: JsonValueCodec[ComplexID]                = JsonCodecMaker.make
given nullableComplexIDCodec: JsonValueCodec[ComplexID | Null] = nullableCodec[ComplexID]

given simpleMessageCodec: JsonValueCodec[Message[SimpleID]]   = JsonCodecMaker.make
given complexMessageCodec: JsonValueCodec[Message[ComplexID]] = JsonCodecMaker.make
