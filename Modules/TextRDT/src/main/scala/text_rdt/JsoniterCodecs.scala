package text_rdt

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

import scala.collection.mutable

given causalIDCodec: JsonValueCodec[CausalID] =
  JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

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

given causalIDBufferCodec: JsonValueCodec[mutable.ArrayBuffer[CausalID]] =
  JsonCodecMaker.make

given causalIDMessageDiffsCodec
    : JsonValueCodec[Seq[(CausalID, mutable.ArrayBuffer[ComplexAVLMessage])]] =
  JsonCodecMaker.make

given sessionDescriptionCodec
    : JsonValueCodec[(String, String, Array[(String, String, Double)])] =
  JsonCodecMaker.make
