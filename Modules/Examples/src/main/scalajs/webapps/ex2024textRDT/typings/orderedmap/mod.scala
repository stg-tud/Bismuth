package webapps.ex2024textRDT.typings.orderedmap

import org.scalablytyped.runtime.StringDictionary

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object mod {

  @JSImport("orderedmap", JSImport.Default)
  @js.native
  object default extends js.Object {
    def from[T](map: StringDictionary[T]): OrderedMap[T] = js.native
  }

  @js.native
  trait OrderedMap[T] extends js.Object
}
