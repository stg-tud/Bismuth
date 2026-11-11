package webapps.ex2022reform.entity

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import rdts.base.*
import webapps.ex2022reform.BasicCodecs.given

case class ContractSchema(
    name: Attribute[String] = Attribute.empty,
    files: Attribute[Seq[String]] = Attribute.empty,
    _exists: Attribute[Boolean] = Attribute.empty,
) extends Entity[ContractSchema]
    derives Lattice,
      Bottom {

  // empty for required fields, default for optional fields
  def default: ContractSchema = ContractSchema(Attribute.empty, Attribute(Seq()), Attribute(true))

  def identifier: Attribute[String] = name

  def withExists(exists: Boolean): ContractSchema =
    this.copy(_exists = _exists.set(exists))

  override def exists: Boolean = _exists.getOrElse(true)

}

object ContractSchema {
  val empty: ContractSchema = ContractSchema()

  given codec: JsonValueCodec[ContractSchema] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
