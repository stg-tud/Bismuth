package webapps.ex2022reform.entity

trait Entity[T] {
  def default: T

  def exists: Boolean

  def identifier: Attribute[String]

  def withExists(exists: Boolean): T
}
