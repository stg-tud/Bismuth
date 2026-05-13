package de.tu_darmstadt.informatik.st.reform.entity

import de.tu_darmstadt.informatik.st.reform.BasicCodecs.myReplicaID
import rdts.base.*
import rdts.datatypes.MultiVersionRegister

case class Attribute[T](register: MultiVersionRegister[T]) {

  def getAll: Seq[T] =
    register.repr.toSeq.sortBy(_._1.time)(using Ordering.Long).map(_._2)

  def option: Option[T] = getAll.headOption

  def get: T = getAll.head

  def getOrElse[U >: T](default: => U): U =
    getAll.headOption.getOrElse(default)

  def hasValue: Boolean = register.repr.nonEmpty

  def set(newValue: T): Attribute[T] = {
    given rdts.base.LocalUid = myReplicaID
    this.copy(register = register.write(newValue))
  }

  def update(f: T => T): Attribute[T] =
    option match {
      case Some(x) => set(f(x))
      case None    => this
    }
}

object Attribute {

  def apply[T](value: T): Attribute[T] = Attribute.empty.set(value)

  def empty[T]: Attribute[T] = Attribute(MultiVersionRegister.empty)

  given bottom[T]: Bottom[Attribute[T]] = Bottom.derived

  given decomposeLattice[T]: Lattice[Attribute[T]] = Lattice.derived
}
