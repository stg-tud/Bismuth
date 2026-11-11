package webapps.ex2022reform.entity

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.*
import webapps.ex2022reform.BasicCodecs.given
import webapps.ex2022reform.entity.Attribute.given

case class User(
    username: Attribute[String] = Attribute.empty,
    role: Attribute[String] = Attribute.empty,
    comment: Attribute[Option[String]] = Attribute.empty,
    _exists: Attribute[Boolean] = Attribute.empty,
) extends Entity[User]
    derives Lattice,
      Bottom {

  // empty for required fields, default for optional fields
  def default: User = User()

  def identifier: Attribute[String] = username

  def withExists(exists: Boolean): User =
    this.copy(_exists = _exists.set(exists))

  override def exists: Boolean = _exists.getOrElse(true)
}

object User {
  val empty: User = User()

  given codec: JsonValueCodec[User] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
