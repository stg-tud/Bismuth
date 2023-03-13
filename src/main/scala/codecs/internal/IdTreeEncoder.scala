package de.tu_darmstadt.stg.daimpl
package codecs.internal

import causality.{EventTree, IdTree}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

private[codecs] object IdTreeEncoder {

  def encode(idTree: IdTree): BitWriter = {
    val bitWriter = BitWriter.empty
    encode(idTree, bitWriter)
    bitWriter
  }

  def encode(idTree: IdTree, bitWriter: BitWriter): Unit = idTree match {
    case IdTree.Leaf(0) => // enc(0) = <<0:2, 0:1>>
      bitWriter.write(0, 2)
      bitWriter.write(0, 1)
    case IdTree.Leaf(1) => // enc(1) = <<0:2, 1:1>>
      bitWriter.write(0, 2)
      bitWriter.write(1, 1)
    case IdTree.Branch(IdTree.Leaf(0), right) => // enc((0,ir)) = <<1:2, enc(ir)>>
      bitWriter.write(1, 2)
      encode(right, bitWriter)
    case IdTree.Branch(left, IdTree.Leaf(0)) => // enc((il, 0)) = <<2:2, enc(il)>>
      bitWriter.write(2, 2)
      encode(left, bitWriter)
    case IdTree.Branch(left, right) => // enc((il, ir)) = <<3:2, enc(il), enc(ir)>>
      bitWriter.write(3, 2)
      encode(left, bitWriter)
      encode(right, bitWriter)
  }

}
