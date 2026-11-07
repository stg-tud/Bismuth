package text_rdt

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

given sideCodec: JsonValueCodec[Side] = JsonCodecMaker.make

given myD3TreeNodeCodec: JsonValueCodec[MyD3TreeNode] =
  JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

given stringBuilderCodec: JsonValueCodec[StringBuilder] =
  new JsonValueCodec[StringBuilder] {

    override def decodeValue(
        in: JsonReader,
        default: StringBuilder
    ): StringBuilder =
      StringBuilder(in.readString(""))

    override def encodeValue(x: StringBuilder, out: JsonWriter): Unit =
      out.writeVal(x.toString)

    override def nullValue: StringBuilder =
      null.asInstanceOf[StringBuilder]
  }

given nullableRIDCodec: JsonValueCodec[RID | Null] =
  new JsonValueCodec[RID | Null] {

    private val optionCodec = JsonCodecMaker.make[Option[RID]]

    override def decodeValue(
        in: JsonReader,
        default: RID | Null
    ): RID | Null =
      optionCodec.decodeValue(in, None).orNull

    override def encodeValue(x: RID | Null, out: JsonWriter): Unit =
      optionCodec.encodeValue(Option(x), out)

    override def nullValue: RID | Null = null
  }

given complexAVLMessageCodec: JsonValueCodec[ComplexAVLMessage] =
  JsonCodecMaker.make

private def nullableCodec[A <: AnyRef](using codec: JsonValueCodec[A]): JsonValueCodec[A | Null] =
  new JsonValueCodec[A | Null] {
    private val optionCodec: JsonValueCodec[Option[A]] = JsonCodecMaker.make

    override def decodeValue(in: JsonReader, default: A | Null): A | Null =
      optionCodec.decodeValue(in, None).orNull

    override def encodeValue(x: A | Null, out: JsonWriter): Unit =
      optionCodec.encodeValue(Option(x), out)

    override def nullValue: A | Null = null
  }

given simpleIDCodec: JsonValueCodec[SimpleID]                  = JsonCodecMaker.make
given nullableSimpleIDCodec: JsonValueCodec[SimpleID | Null]   = nullableCodec[SimpleID]
given complexIDCodec: JsonValueCodec[ComplexID]                = JsonCodecMaker.make
given nullableComplexIDCodec: JsonValueCodec[ComplexID | Null] = nullableCodec[ComplexID]

given simpleMessageCodec: JsonValueCodec[Message[SimpleID]]   = JsonCodecMaker.make
given complexMessageCodec: JsonValueCodec[Message[ComplexID]] = JsonCodecMaker.make
