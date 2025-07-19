package com.daimpl.lib

case class ConflictableValue[A](elements: Set[A]) {

  def hasNoConflicts: Boolean = !hasConflicts

  def hasConflicts: Boolean = elements.size > 1

  def getIfNoConflicts: Option[A] =
    if (hasNoConflicts) elements.headOption
    else None

  def formatConflicts(separator: String = "/"): String =
    elements.mkString(separator)

  def getFirstOrEmpty: Option[A] = elements.headOption

  def toList: List[A] = elements.toList
}

object ConflictableValue {
  def empty[A]: ConflictableValue[A] = ConflictableValue(Set.empty[A])

  def fromSet[A](set: Set[A]): ConflictableValue[A] = ConflictableValue(set)
}
