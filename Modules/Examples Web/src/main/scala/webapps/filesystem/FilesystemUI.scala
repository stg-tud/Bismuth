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
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.Function.const
import scala.scalajs.js.timers.setTimeout

case class MoveToParent(entry: Dot, parent: Dot)

val timeZone = {
  try {
    ZoneId.systemDefault()
  } catch {
    case _: Exception => ZoneId.of("UTC")
  }
}

object Icons {
  val file   = "assets/file.svg"
  val folder = "assets/folder.svg"
}

enum EntryType:
  case File
  case Folder

case class MoveEntry(id: Dot, parent: Dot)

case class Entry(val id: Dot, val name: LWW[String], val ty: EntryType, val mtime: Long = System.currentTimeMillis()) {
  val onClick       = Event.fromCallback(onclick := Event.handle)
  val onDoubleClick = Event.fromCallback(ondblclick := Event.handle)
  val onDragStart   = Event.fromCallback(ondragstart := Event.handle)
  val onDragOver    = Event.fromCallback(ondragover := Event.handle)
  val onDrop        = Event.fromCallback(ondrop := Event.handle)
  val onEntryDrop   = onDrop.event.map { e =>
    val de        = e.asInstanceOf[DragEvent]
    val data      = de.dataTransfer.getData("text/plain")
    val parts     = data.split(":")
    val place     = Uid.predefined(parts(0))
    val time      = parts(1).toLong
    val targetDot = Dot(place, time)
    MoveEntry(targetDot, id)
  }
  val onKeyDown       = Event.fromCallback(onkeydown := Event.handle)
  val onReturnKeyDown = onKeyDown.event.filter { e =>
    val ke = e.asInstanceOf[dom.KeyboardEvent]
    ke.key == "Enter" && !ke.target.isInstanceOf[dom.html.Input]
  }

  val editText: Event.CBR[dom.Event, dom.html.Input] = Event.fromCallback {
    input(
      `class`   := "entry-name-edit",
      `type`    := "text",
      onkeydown := Event.handle[dom.Event],
    ).render
  }

  val onCancelEdit = editText.event.filter { e =>
    val ke = e.asInstanceOf[dom.KeyboardEvent]
    ke.key == "Escape"
  }

  val onConfirmEdit = editText.event.filter { e =>
    val ke = e.asInstanceOf[dom.KeyboardEvent]
    ke.key == "Enter"
  }

  val editTextValue = editText.event.map { (e: dom.Event) =>
    val input = e.target.asInstanceOf[Input]
    input.value.trim
  }.hold("")

  val editInput = editText.data.reattach(Signal { value := name.value })

  val changeEditing =
    (onReturnKeyDown `map` const(true)) || (onConfirmEdit `map` const(false)) || (onCancelEdit `map` const(false))
  val isEditing = changeEditing.hold(false)

  val onRename: Event[String] = changeEditing.filter(!_).map { _ => editTextValue.value }

  val nameElement = isEditing.map { editing =>
    if editing then {
      editInput
    } else {
      span(`class` := "entry-name-text", name.value).render
    }
  }

  def toTag(id: Dot, isSelected: Signal[Boolean]): LI = {
    isEditing.observe { editing =>
      if editing then {
        setTimeout(0) { editInput.focus() }; ()
      }
    }

    onDragStart.event.observe(e => {
      val de = e.asInstanceOf[DragEvent]
      de.dataTransfer.setData("text/plain", id.place.delegate + ":" + id.time.toString)
    })

    onDragOver.event.observe(e => {
      val de = e.asInstanceOf[DragEvent]
      de.preventDefault()
    })

    val date = Instant.ofEpochMilli(
      mtime
    ).atZone(timeZone).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

    li(
      `class`     := "filesystem-entry",
      `draggable` := "true",
      tabindex    := "0",
      div(
        `class` := "entry-content view",
        div(
          `class` := "entry-icon",
          img(
            src := (ty match {
              case EntryType.File   => Icons.file
              case EntryType.Folder => Icons.folder
            })
          )
        ),
        div(`class` := "entry-name").render.reattach(nameElement),
        div(`class` := "entry-date", date)
      ),
      onClick.data,
      onDoubleClick.data,
      onDragStart.data,
      onDragOver.data,
      onDrop.data,
      onKeyDown.data
    ).render.reattach(Signal {
      var classes = "filesystem-entry"
      if isSelected.value then classes += " selected-entry"
      if isEditing.value then classes += " editing"

      if isSelected.value then { (elem: dom.Element) =>
        elem.setAttribute("class", classes)
        elem.asInstanceOf[LI].focus()
      } else { (elem: dom.Element) =>
        elem.setAttribute("class", classes)
      }
    })
  }
}

object Entry {
  def file(id: Dot, name: String): Entry   = Entry(id, LWW.now(name), EntryType.File)
  def folder(id: Dot, name: String): Entry = Entry(id, LWW.now(name), EntryType.Folder)

  given lexicographicOrdering: Ordering[ReplicatedTree.Node[Entry]] = Ordering.by(_.value.name.value)
}

//todo: Ideally these should be deterministic per machine somehow
type ReplicaId = Uid

case class FilesystemState(
    val tree: ReplicatedTree[Entry],
    val selections: Map[ReplicaId, LWW[Option[Dot]]] = Map.empty,
    val locations: Map[ReplicaId, LWW[Dot]] = Map.empty
) {
  def addEntry(parent: Dot, entry: Dot => Entry)(using LocalUid): FilesystemState = {
    FilesystemState(tree = tree.insertWith(parent, dot => entry(dot)))
  }

  def renameEntry(entry: Dot, newName: String)(using LocalUid): FilesystemState = {
    tree.node(entry) match {
      case Some(n) if n.value.name.value != newName =>
        FilesystemState(
          tree = tree.update(entry, n.value.copy(name = LWW.now(newName), mtime = System.currentTimeMillis()))
        )
      case _ => FilesystemState.empty
    }
  }

  def moveToParent(entry: Dot, parent: Dot)(using LocalUid): FilesystemState = {
    tree.node(parent) match
      case Some(n) if n.value.ty == EntryType.Folder => FilesystemState(tree = tree.move(entry, parent))
      case _                                         => FilesystemState.empty
  }

  def isSelected(using LocalUid)(entry: Dot): Boolean = {
    selections.get(LocalUid.replicaId).map(_.value).flatten == Some(entry)
  }

  def selection(using LocalUid): Dot = {
    selections
      .get(LocalUid.replicaId)
      .map(_.value)
      .flatten
      .getOrElse(ReplicatedTree.rootDot)
  }

  def clearAll()(using LocalUid): FilesystemState = {
    FilesystemState(
      tree = tree.clear(),
      selections = Map(),
      locations = Map(),
    )
  }

  def location(using LocalUid): Dot = {
    locations.get(LocalUid.replicaId).map(_.value).getOrElse(ReplicatedTree.rootDot)
  }

  def setLocation(target: Dot)(using LocalUid): FilesystemState = {
    if location == target then FilesystemState.empty
    else
      FilesystemState(
        tree = ReplicatedTree.empty,
        selections = selections + (LocalUid.replicaId -> LWW.now(None)),
        locations = locations + (LocalUid.replicaId   -> LWW.now(target))
      )
  }

  def markSelected(entry: Dot)(using LocalUid): FilesystemState = {
    if selection == entry then FilesystemState.empty
    else
      FilesystemState(
        tree = ReplicatedTree.empty,
        selections = selections + (LocalUid.replicaId -> LWW.now(Some(entry)))
      )
  }
}

object FilesystemState {
  def empty: FilesystemState = FilesystemState(ReplicatedTree.empty[Entry])

  given Lattice[FilesystemState] = Lattice.derived
  given Bottom[FilesystemState]  = Bottom.provide(FilesystemState(ReplicatedTree.empty[Entry]))
}

type State = UndoRedoReplica[FilesystemState];

enum Direction:
  case Up
  case Down
  case Left
  case Right

class FilesystemUI(val storagePrefix: String, val replicaId: LocalUid) {
  def getContents(): Div = {
    given LocalUid = replicaId

    val goToParent            = Event.fromCallback(button("Go to parent", onclick := Event.handle))
    val addRandomFileButton   = Event.fromCallback(button("Add random file", onclick := Event.handle))
    val addRandomFolderButton = Event.fromCallback(button("Add random folder", onclick := Event.handle))
    val deleteAllButton       = Event.fromCallback(button("Delete all", onclick := Event.handle))
    val undoButton            = Event.fromCallback(button("Undo", onclick := Event.handle))
    val redoButton            = Event.fromCallback(button("Redo", onclick := Event.handle))

    val markSelected = Interaction[State, Dot]
      .executes { (s: State, e) => s.mod(_.markSelected(e)) }

    val setLocation = Interaction[State, Dot]
      .executes { (s: State, d) => s.mod(_.setLocation(d)) }

    val onDropEntry = Interaction[State, MoveEntry]
      .executes { (s: State, e) => s.mod(_.moveToParent(e.id, e.parent)) }

    val renameEntry = Interaction[State, (Dot, String)]
      .executes { (s: State, e) => s.mod(_.renameEntry(e._1, e._2)) }

    def events[T](s: State)(mapper: ReplicatedTree.Node[Entry] => Event[T]): Event[T] = {
      val events = s.state.tree.nodes.map(mapper)

      Event.Impl.static(events.toSeq*) { st =>
        events.map(st.dependStatic)
          .collectFirst { case Some(e) => e }
      }
    }

    // val onKeyDown             = Event.fromCallback(onkeydown := Event.handle)
    // val onNavigateToDirection = onKeyDown.event.map { e =>
    //   val k = e.asInstanceOf[dom.KeyboardEvent]
    //   k.key match {
    //     case "ArrowUp"    => Some(Direction.Up)
    //     case "ArrowDown"  => Some(Direction.Down)
    //     case "ArrowLeft"  => Some(Direction.Left)
    //     case "ArrowRight" => Some(Direction.Right)
    //     case _            => None
    //   }
    // }.flatten

    val stateRDT: Signal[State] = {
      Storing.storedAs(storagePrefix, UndoRedoReplica.empty[FilesystemState]) { init =>
        FilesystemDataManager.hookup(
          init,
        ) { (init, branch) =>
          Fold(UndoRedoReplica.empty[FilesystemState])(
            FilesystemDataManager.resetBuffer,
            addRandomFileButton.event.branch { _ =>
              val name = scala.util.Random.alphanumeric.take(8).mkString
              current.mod((s) => s.addEntry(s.location, dot => Entry.file(dot, name)))
            },
            addRandomFolderButton.event.branch { _ =>
              val name = scala.util.Random.alphanumeric.take(8).mkString
              current.mod((s) => s.addEntry(s.location, dot => Entry.folder(dot, name)))
            },
            deleteAllButton.event.branch { _ => current.mod(_.clearAll()) },
            markSelected.actWith[State](events(current)((n) => n.value.onClick.event.map(_ => n.dot))),
            setLocation.actWith[State](events(current)((n) =>
              n.value.onDoubleClick.event.filter(_ => n.value.ty == EntryType.Folder).map(_ => n.dot)
            )),
            onDropEntry.actWith[State](events(current)((n) => n.value.onEntryDrop)),
            renameEntry.actWith[State](events(current)((n) =>
              n.value.onRename.map { s => (n.dot, s) }
            )),
            // onNavigateToDirection.branch { d =>
            //   d match {
            //     case Direction.Left => {
            //       val node = current.state.tree.node(current.state.location)
            //       node match
            //         case Some(node) => current.mod(_.setLocation(node.parent))
            //         case None       => current
            //     }
            //     case Direction.Right => {
            //       val selected     = current.state.selection
            //       val selectedNode = current.state.tree.node(selected)
            //       selectedNode match {
            //         case Some(n) if n.value.ty == EntryType.Folder =>
            //           current.mod(_.setLocation(selected))
            //         case _ => current
            //       }
            //     }
            //     case Direction.Down | Direction.Up => {
            //       val selection     = current.state.selection
            //       val activeEntries =
            //         current.state.tree.children(current.state.location).toList.sorted(using Entry.lexicographicOrdering)
            //       val currentIndex = activeEntries.indexWhere(_.dot == selection)
            //       val targetIndex  = d match {
            //         case Direction.Up   => Math.max(currentIndex - 1, 0)
            //         case Direction.Down => Math.min(currentIndex + 1, activeEntries.size - 1)
            //         case _              => throw new IllegalStateException()
            //       }
            //       val target = activeEntries(targetIndex).dot
            //       current.mod(_.markSelected(target))
            //     }
            //   }
            // },
            goToParent.event.branch { _ =>
              val node = current.state.tree.node(current.state.location).get
              current.mod(_.setLocation(node.parent))
            },
            undoButton.event.branch { _ =>
              current.undo()
            },
            redoButton.event.branch { _ =>
              current.redo()
            },
            branch
          )
        }
      }
    }

    val state: Signal[FilesystemState] = stateRDT.map(_.state)

    val parent: Signal[Option[ReplicatedTree.Node[Entry]]] = state.map { s =>
      s.tree.node(s.location)
    }

    val entryNodes: Signal[List[ReplicatedTree.Node[Entry]]] =
      state.map((s) => s.tree.children(s.location).toList.sorted(using Entry.lexicographicOrdering))

    val entriesTags: Signal[Seq[LI]] = entryNodes.map { entries =>
      entries.map((n) => {
        n.value.toTag(n.dot, state.map(_.isSelected(n.dot)))
      })
    }

    val entryList =
      ul(`class` := "filesystem-list").render.reattach(entriesTags)

    div(
      autofocus,
      h1().render.reattach(
        Signal {
          span(parent.value.map(_.value.name.value).getOrElse("/")).render
        }
      ),
      div(
        `class` := "filesystem-container",
        undoButton.data.render.reattach(
          DomHelper.enabledWhen(stateRDT.map(_.canUndo))
        ),
        redoButton.data.render.reattach(
          DomHelper.enabledWhen(stateRDT.map(_.canRedo))
        ),
        goToParent.data.render,
        addRandomFileButton.data.render,
        addRandomFolderButton.data.render,
        deleteAllButton.data.render,
      ),
      entryList,
      // onKeyDown.data,
    ).render
  }
}

given RangeSplice[dom.Element, Modifier] with {
  override def splice(anchor: dom.Element, range: dom.Range, value: Modifier): Unit =
    anchor match
      case elem: dom.Element => value.applyTo(elem)
}

given [A <: dom.Element]: RangeSplice[A, A => Unit] with {
  override def splice(anchor: A, range: dom.Range, value: A => Unit): Unit =
    anchor match
      case elem: A => value.apply(elem)
}

object DomHelper {
  def enabledWhen(enabled: Signal[Boolean]) = {
    Signal {
      if enabled.value then { (elem: dom.Element) =>
        elem.removeAttribute("disabled")
      } else { (elem: dom.Element) =>
        elem.setAttribute("disabled", "true")
      }
    }
  }
}
