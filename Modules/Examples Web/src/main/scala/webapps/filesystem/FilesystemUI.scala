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
import lore.Parser._var

case class MoveToParent(entry: Dot, parent: Dot)

object Icons {
  val file   = "assets/file.svg"
  val folder = "assets/folder.svg"
}

enum EntryType:
  case File
  case Folder

case class Entry(val name: String, val ty: EntryType) {
  val onClick       = Event.fromCallback(onclick := Event.handle)
  val onDoubleClick = Event.fromCallback(ondblclick := Event.handle)
  val keyDown       = Event.fromCallback(onkeydown := Event.handle)

  def toTag(isSelected: Signal[Boolean]): LI = {
    val selected = isSelected.map((s) => `class` := (if s then "selected-list-item-bg" else ""))

    li(
      `class` := "filesystem-entry",
      img(
        src := (ty match {
          case EntryType.File   => Icons.file
          case EntryType.Folder => Icons.folder
        })
      ),
      p(name),
      onClick.data,
      onDoubleClick.data,
      keyDown.data
    ).render.reattach(selected)

  }
}

object Entry {
  given lexicographicOrdering: Ordering[ReplicatedTree.Node[Entry]] = Ordering.by(_.value.name)
}

type ReplicaId = Uid

case class FilesystemState(
    val tree: ReplicatedTree[Entry],
    val selected: ObserveRemoveMap[ReplicaId, LWW[Set[Dot]]] = ObserveRemoveMap.empty,
    val locations: ObserveRemoveMap[ReplicaId, LWW[Dot]] = ObserveRemoveMap.empty
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

  def location(using LocalUid): Dot = {
    locations.get(LocalUid.replicaId).map(_.value).getOrElse(ReplicatedTree.rootDot)
  }

  def setLocation(location: Dot)(using LocalUid): FilesystemState = {
    FilesystemState(
      tree = ReplicatedTree.empty,
      selected = selected.update(LocalUid.replicaId, LWW.now(Set.empty)),
      locations = locations.transform(LocalUid.replicaId) { _ =>
        Some(LWW.now(location))
      }
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

    val goToParent            = Event.fromCallback(button("Go to parent", onclick := Event.handle))
    val addRandomFileButton   = Event.fromCallback(button("Add random file", onclick := Event.handle))
    val addRandomFolderButton = Event.fromCallback(button("Add random folder", onclick := Event.handle))
    val deleteAllButton       = Event.fromCallback(button("Delete all", onclick := Event.handle))

    val addEntryEvent = entryInputTag.inputEntered.map { name =>
      Entry(name, EntryType.File)
    }

    val toggleSelection = Interaction[State, Dot]
      .executes { (s: State, e) => s.mod(_.toggleSelection(e)) }

    val setLocation = Interaction[State, Dot]
      .executes { (s: State, d) => s.mod(_.setLocation(d)) }

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
          addEntryEvent.branch { e => current.mod((s) => s.addEntry(s.location, e)) },
          addRandomFileButton.event.branch { _ =>
            val name = scala.util.Random.alphanumeric.take(8).mkString
            current.mod((s) => s.addEntry(s.location, Entry(name, EntryType.File)))
          },
          addRandomFolderButton.event.branch { _ =>
            val name = scala.util.Random.alphanumeric.take(8).mkString
            current.mod((s) => s.addEntry(s.location, Entry(name, EntryType.Folder)))
          },
          deleteAllButton.event.branch { _ => current.mod(_.clearAll()) },
          toggleSelection.actWith[State](events(current)((n) => n.value.onClick.event.map(_ => n.dot))),
          setLocation.actWith[State](events(current)((n) =>
            n.value.onDoubleClick.event.filter(_ => n.value.ty == EntryType.Folder).map(_ => n.dot)
          )),
          goToParent.event.branch { _ =>
            val node = current.state.tree.node(current.state.location).get
            current.mod(_.setLocation(node.parent))
          },
        )
      }

    val parent: Signal[Option[ReplicatedTree.Node[Entry]]] = stateRDT.map { s =>
      s.state.tree.node(s.state.location)
    }

    val entryNodes: Signal[List[ReplicatedTree.Node[Entry]]] =
      stateRDT.map((s) => s.state.tree.children(s.state.location).toList.sorted(using Entry.lexicographicOrdering))

    val entriesTags: Signal[Seq[LI]] = entryNodes.map { entries =>
      entries.map((n) => n.value.toTag(stateRDT.map(_.state.isEntrySelected(n.dot).isDefined)))
    }

    val entryList = ul(
      `class` := "todo-list",
    ).render.reattach(entriesTags)

    div(
      h1().render.reattach(
        Signal {
          span(parent.value.map(_.value.name).getOrElse("/")).render
        }
      ),
      div(
        `class` := "filesystem-list ",
        entryInputTag,
        goToParent.data.render,
        addRandomFileButton.data.render,
        addRandomFolderButton.data.render,
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
