package webapps.ex2024textRDT.typings.prosemirrorKeymap

import org.scalablytyped.runtime.StringDictionary
import webapps.ex2024textRDT.typings.prosemirrorState.mod.{Command, Plugin}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object mod {

  @JSImport("prosemirror-keymap", JSImport.Namespace)
  @js.native
  val ^ : js.Any = js.native

  inline def keymap(bindings: StringDictionary[Command]): Plugin[Any] =
    ^.asInstanceOf[js.Dynamic].applyDynamic("keymap")(bindings.asInstanceOf[js.Any]).asInstanceOf[Plugin[Any]]
}
