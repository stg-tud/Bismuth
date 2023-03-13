package de.tu_darmstadt.stg.daimpl
package codecs

import causality.EventTree
import causality.EventTree.{Branch, Leaf}
import causality.EventTreeGenerators.genEventTree
import codecs.{EncoderSpec, FastEventTreeEncoder}

class FastEventTreeEncoderSpec extends EncoderSpec[EventTree] {
  def printByteArray(array: Array[Byte]): Unit = {
    val strings = array.map(byte => {
      val (left, right) = Integer.toBinaryString(byte & 0xff).padTo(8, '0').splitAt(4)
      left + '_' + right
    })
    println(strings.mkString("Array(", ", ", ")"))
  }
}
