package webapps.filesystem

import lore.dsl.{Interaction, InteractionWithExecutes, Invariant}
import org.scalajs.dom.html.{Div, Input}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.syntax.DeltaBuffer
import reactives.core.CreationTicket
import reactives.default.*
import reactives.extra.Tags.*
import reactives.operator.Event.CBR
import scalatags.JsDom.all.*
import webapps.Storing

class FilesystemUI(val storagePrefix: String, val replicaId: Uid) {
  def getContents(): Div = {
    div(
      h1("Filesystem")
    ).render
  }
}
