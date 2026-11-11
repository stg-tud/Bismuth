package webapps.ex2022reform.entity

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import rdts.base.*
import webapps.ex2022reform.BasicCodecs.given

case class Contract(
    contractAssociatedProject: Attribute[String] = Attribute.empty,
    contractAssociatedHiwi: Attribute[String] = Attribute.empty,
    contractAssociatedPaymentLevel: Attribute[String] = Attribute.empty,
    contractAssociatedSupervisor: Attribute[String] = Attribute.empty,
    contractStartDate: Attribute[Long] = Attribute.empty,
    contractEndDate: Attribute[Long] = Attribute.empty,
    contractType: Attribute[String] = Attribute.empty,
    contractHoursPerMonth: Attribute[Int] = Attribute.empty,
    isDraft: Attribute[Boolean] = Attribute.empty,
    requiredDocuments: Attribute[Seq[String]] = Attribute.empty,
    isSigned: Attribute[Boolean] = Attribute.empty,
    isSubmitted: Attribute[Boolean] = Attribute.empty,
    reminderSentDate: Attribute[Long] = Attribute.empty,
    contractSentDate: Attribute[Long] = Attribute.empty,
    letterSentDate: Attribute[Long] = Attribute.empty,
    _exists: Attribute[Boolean] = Attribute.empty,
) extends Entity[Contract]
    derives Lattice,
      Bottom {

  def identifier: Attribute[String] = contractAssociatedHiwi

  def withExists(exists: Boolean): Contract =
    this.copy(_exists = _exists.set(exists))

  override def exists: Boolean = _exists.getOrElse(true)

  def default: Contract = Contract()

}

object Contract {
  val empty: Contract = Contract()

  given codec: JsonValueCodec[Contract] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

}
