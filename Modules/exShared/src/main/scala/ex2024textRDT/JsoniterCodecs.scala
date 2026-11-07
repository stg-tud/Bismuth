package ex2024textRDT

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

given sideCodec: JsonValueCodec[Side] = JsonCodecMaker.make

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

given nullableRIDCodec: JsonValueCodec[RID | Null] = nullableCodec[RID](using JsonCodecMaker.make)

def nullableCodec[A <: AnyRef](using codec: JsonValueCodec[A]): JsonValueCodec[A | Null] =
  new JsonValueCodec[A | Null] {
    private val optionCodec: JsonValueCodec[Option[A]] = JsonCodecMaker.make

    override def decodeValue(in: JsonReader, default: A | Null): A | Null =
      optionCodec.decodeValue(in, None).orNull

    override def encodeValue(x: A | Null, out: JsonWriter): Unit =
      optionCodec.encodeValue(Option(x), out)

    override def nullValue: A | Null = null
  }
