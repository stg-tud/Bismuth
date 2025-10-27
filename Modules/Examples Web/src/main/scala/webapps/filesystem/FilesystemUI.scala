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
import rdts.datatypes.ReplicatedTree
import rdts.time.Dot
import org.scalajs.dom
import org.scalajs.dom.Element
import scalatags.generic.AttrPair
import reactives.operator.Event.CBR
import rdts.datatypes.ObserveRemoveMap
import rdts.datatypes.LastWriterWins as LWW

case class MoveToParent(entry: Dot, parent: Dot)

// case class Entry(val id: Dot, val name: String) {
//   private val item: CBR[DragEvent, LI] = Event.fromCallback {
//     li(
//       name,
//       draggable   := true,
//       ondragstart := { (e: DragEvent) =>
//         val s = id.place.toString + "," + id.time
//         e.dataTransfer.setData("text/json", s)
//       },
//       ondragenter := { (e: DragEvent) =>
//         e.currentTarget.asInstanceOf[LI].classList.add("drag-over")
//       },
//       ondragover := { (e: DragEvent) =>
//         e.preventDefault()
//       },
//       ondragleave := { (e: DragEvent) =>
//         e.currentTarget.asInstanceOf[LI].classList.remove("drag-over")
//       },
//       ondrop := Event.handle
//     ).render
//   }

//   val dropEvent: Event[MoveToParent] = item.event.map { e =>
//     e.currentTarget.asInstanceOf[LI].classList.remove("drag-over")
//     val data  = e.dataTransfer.getData("text/json")
//     val parts = data.split(",")

//     val dot = Dot(Uid.predefined(parts(0)), Time(parts(1).toLong))
//     MoveToParent(dot, id)
//   }

//   def toTag: LI = item.data
// }

case class Entry(val name: String) {
  val onClick = Event.fromCallback(onclick := Event.handle)

  def toTag(isSelected: Signal[Boolean]): LI = {
    val selected = isSelected.map((s) => `class` := (if s then "selected-list-item-bg" else ""))

    li(
      name,
      onClick.data,
    ).render.reattach(selected)
  }
}

object Entry {
  given lexicographicOrdering: Ordering[ReplicatedTree.Node[Entry]] = Ordering.by(_.value.name)
}

case class FilesystemState(
    val tree: ReplicatedTree[Entry],
    val selected: ObserveRemoveMap[Uid, LWW[Set[Dot]]] = ObserveRemoveMap.empty
) {
  def addEntry(parent: Dot, entry: Entry)(using LocalUid): FilesystemState = {
    FilesystemState(tree = tree.insert(parent, entry))
  }

  def moveToParent(entry: Dot, parent: Dot)(using LocalUid): FilesystemState = {
    FilesystemState(tree = tree.move(entry, parent))
  }

  def isEntrySelected(entry: Dot): Option[Uid] = {
    selected.entries.find(_._2.value.contains(entry)).map(_._1)
  }

  def clearAll()(using LocalUid): FilesystemState = {
    FilesystemState(
      tree = tree.clear(),
      selected = selected.clear()
    )
  }

  def toggleSelection(entry: Dot)(using LocalUid): FilesystemState = {
    FilesystemState(
      tree = ReplicatedTree.empty,
      selected = selected.transform(LocalUid.replicaId) { s =>
        val set    = s.map(_._2).getOrElse(Set.empty)
        val newSet = if set.contains(entry) then set - entry else set + entry
        Some((LWW.now(newSet)))
      }
    )
  }
}

object FilesystemState {
  given Lattice[FilesystemState] = Lattice.derived
  given Bottom[FilesystemState]  = Bottom.provide(FilesystemState(ReplicatedTree.empty[Entry]))
}

type State = DeltaBuffer[FilesystemState];

class FilesystemUI(val storagePrefix: String, val replicaId: LocalUid) {
  def getContents(): Div = {
    given LocalUid = replicaId

    val entryInputTag: Input = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "Add new file...",
      autofocus   := "autofocus",
      `type`      := "text"
    ).render

    val addRandomFileButton = Event.fromCallback(button("Add random entry", onclick := Event.handle))
    val deleteAllButton     = Event.fromCallback(button("Delete all", onclick := Event.handle))

    val addEntryEvent = entryInputTag.inputEntered.map { name =>
      Entry(name)
    }

    val activeParent: Signal[Dot] = Signal(ReplicatedTree.rootDot)

    val toggleSelection = Interaction[State, Dot]
      .executes { (s: State, e) => s.mod(_.toggleSelection(e)) }

    def events[T](s: State)(mapper: ReplicatedTree.Node[Entry] => Event[T]): Event[T] = {
      val events = s.state.tree.nodes.map(mapper)

      Event.Impl.static(events.toSeq*) { st =>
        events.map(st.dependStatic)
          .collectFirst { case Some(e) => e }
      }
    }

    val stateRDT: Signal[State] =
      Storing.storedAs(storagePrefix, DeltaBuffer(FilesystemState(tree = ReplicatedTree.empty[Entry]))) { init =>
        Fold(init)(
          addEntryEvent.branch { e => current.mod(_.addEntry(activeParent.now, e)) },
          addRandomFileButton.event.branch { _ =>
            val name = "file_" + scala.util.Random.alphanumeric.take(8).mkString
            current.mod(_.addEntry(activeParent.now, Entry(name)))
          },
          deleteAllButton.event.branch { _ => current.mod(_.clearAll()) },
          toggleSelection.actWith[State](events(current)((n) => n.value.onClick.event.map(_ => n.dot))),
        )
      }

    val entryNodes: Signal[List[ReplicatedTree.Node[Entry]]] =
      stateRDT.map(_.state.tree.children(activeParent.now).toList.sorted(using Entry.lexicographicOrdering))

    val entriesTags: Signal[Seq[LI]] = entryNodes.map { entries =>
      entries.map((n) => n.value.toTag(stateRDT.map(_.state.isEntrySelected(n.dot).isDefined)))
    }

    val entryList = ul(
      `class` := "todo-list",
    ).render.reattach(entriesTags)

    div(
      h1("Filesystem"),
      div(
        `class` := "filesystem-list",
        entryInputTag,
        addRandomFileButton.data.render,
        deleteAllButton.data.render
      ),
      entryList,
    ).render
  }
}

given RangeSplice[dom.Element, Modifier] with {
  override def splice(anchor: dom.Element, range: dom.Range, value: Modifier): Unit =
    anchor match
      case elem: dom.Element => value.applyTo(elem)
}
