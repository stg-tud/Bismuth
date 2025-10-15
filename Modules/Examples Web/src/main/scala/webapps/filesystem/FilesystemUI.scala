package webapps.filesystem

import lore.dsl.{Interaction, InteractionWithExecutes, Invariant}
import org.scalajs.dom.DragEvent;
import org.scalajs.dom.html.{Div, Input, LI}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.syntax.DeltaBuffer
import reactives.core.CreationTicket
import reactives.default.*
import reactives.extra.Tags.*
import reactives.operator.Event.CBR
import scalatags.JsDom.all.*
import webapps.Storing
import rdts.experiments.UndoRedoReplica
import rdts.datatypes.RemoveWinsArray
import rdts.base.Bottom
import webapps.filesystem.Codecs.codecRGA
import rdts.base.Lattice

case class MoveToBefore(entry: Uid, before: Uid)

case class Entry(val id: Uid, val name: String) {
  private val item: CBR[DragEvent, LI] = Event.fromCallback {
    li(
      name,
      draggable   := true,
      ondragstart := { (e: DragEvent) =>
        e.dataTransfer.setData("text/plain", Uid.unwrap(id))
      },
      ondragenter := { (e: DragEvent) =>
        e.currentTarget.asInstanceOf[LI].classList.add("drag-over")
      },
      ondragover := { (e: DragEvent) =>
        e.preventDefault()
      },
      ondragleave := { (e: DragEvent) =>
        e.currentTarget.asInstanceOf[LI].classList.remove("drag-over")
      },
      ondrop := Event.handle
    ).render
  }

  val dropEvent: Event[MoveToBefore] = item.event.map { e =>
    e.currentTarget.asInstanceOf[LI].classList.remove("drag-over")
    val data = e.dataTransfer.getData("text/plain")
    MoveToBefore(Uid.predefined(data), id)
  }

  def toTag: LI = item.data
}

object Entry {
  given Bottom[Entry] = Bottom.provide(Entry(Uid.zero, ""))
}

case class FilesystemState(val entries: RemoveWinsArray[Entry]) {
  def addEntry(entry: Entry)(using LocalUid): FilesystemState = {
    FilesystemState(entries = entries.append(entry))
  }

  def moveTo(entry: Uid, before: Uid)(using LocalUid): FilesystemState = {
    val list      = entries.toList
    val currentIx = list.indexWhere(_.id == entry)
    val targetIx  = list.indexWhere(_.id == before)
    FilesystemState(entries = entries.move(currentIx, targetIx))
  }
}

object FilesystemState {
  given Lattice[FilesystemState] = Lattice.derived
  given Bottom[FilesystemState]  = Bottom.derived
}

type State = DeltaBuffer[FilesystemState];

class FilesystemUI(val storagePrefix: String, val replicaId: LocalUid) {
  def getContents(): Div = {
    given LocalUid = replicaId

    val entryInputTag: Input = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "Add new entry",
      autofocus   := "autofocus",
      `type`      := "text"
    ).render

    val addEntryEvent = entryInputTag.inputEntered.map { name =>
      Entry(Uid.gen(), name)
    }

    val addEntry = Interaction[State, Entry]
      .executes { (s: State, e) => s.mod(_.addEntry(e)) }
      .ensures { (s: State, e) => s.state.entries.toList.contains(e) }

    val dropEntry = Interaction[State, MoveToBefore]
      .executes { (s: State, e) => s.mod(_.moveTo(e.entry, e.before)) }

    def events[T](s: State)(mapper: Entry => Event[T]): Event[T] = {
      val events = s.state.entries.toList.map(mapper)

      Event.Impl.static(events.toSeq*) { st =>
        events.map(st.dependStatic)
          .collectFirst { case Some(e) => e }
      }
    }

    val stateRDT: Signal[State] =
      Storing.storedAs(storagePrefix, DeltaBuffer(FilesystemState(entries = RemoveWinsArray.empty[Entry]))) { init =>
        Fold(init)(
          addEntry.actsOn(addEntryEvent).foldInto,
          dropEntry.actWith[State](events(current)(_.dropEvent)),
        )
      }

    val entriesList: Signal[List[Entry]] = stateRDT.map(_.state.entries.toList)

    val entriesTags: Signal[Seq[LI]] = entriesList.map { entries =>
      entries.map(_.toTag)
    }

    val entryList = ul(
      `class` := "todo-list",
    ).render.reattach(entriesTags)

    div(
      h1("Filesystem"),
      entryInputTag,
      entryList
    ).render
  }
}
