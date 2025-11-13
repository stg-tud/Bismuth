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

case class MoveEntry(id: Dot, parent: Dot)

case class FsEntryView(
    val state: FsEntry
) {
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
    MoveEntry(targetDot, state.id)
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

  val editInput = editText.data.reattach(Signal { value := state.name.value })

  val changeEditing =
    (onReturnKeyDown `map` const(true)) || (onConfirmEdit `map` const(false)) || (onCancelEdit `map` const(false))
  val isEditing = changeEditing.hold(false)

  val onRename: Event[String] = changeEditing.filter(!_).map { _ => editTextValue.value }

  val nameElement = isEditing.map { editing =>
    if editing then {
      editInput
    } else {
      span(`class` := "entry-name-text", state.name.value).render
    }
  }

  def toTag(isSelected: Signal[Boolean]): LI = {
    isEditing.observe { editing =>
      if editing then {
        setTimeout(0) { editInput.focus() }; ()
      }
    }

    onDragStart.event.observe(e => {
      val de = e.asInstanceOf[DragEvent]
      de.dataTransfer.setData("text/plain", state.id.place.delegate + ":" + state.id.time.toString)
    })

    onDragOver.event.observe(e => {
      val de = e.asInstanceOf[DragEvent]
      de.preventDefault()
    })

    val date = Instant.ofEpochMilli(
      state.mtime
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
            src := (state.ty match {
              case FsEntryType.File   => Icons.file
              case FsEntryType.Folder => Icons.folder
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

//todo: Ideally these should be deterministic per machine somehow
type ReplicaId = Uid

case class FsEntry(
    val id: Dot,
    val name: LWW[String],
    val ty: FsEntryType,
    val mtime: Long = System.currentTimeMillis()
)

object FsEntry {
  def file(id: Dot, name: String): FsEntry   = FsEntry(id, LWW.now(name), FsEntryType.File)
  def folder(id: Dot, name: String): FsEntry = FsEntry(id, LWW.now(name), FsEntryType.Folder)

  given lexicographicOrdering: Ordering[ReplicatedTree.Node[FsEntry]] = Ordering.by(_.value.name.value)
}

enum FsEntryType:
  case File
  case Folder

case class FilesystemState(
    val tree: ReplicatedTree[FsEntry],
    val selections: Map[ReplicaId, LWW[Option[Dot]]] = Map.empty,
    val locations: Map[ReplicaId, LWW[Dot]] = Map.empty
) {
  def addEntry(parent: Dot, entry: Dot => FsEntry)(using LocalUid): FilesystemState = {
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
      case Some(n) if n.value.ty == FsEntryType.Folder => FilesystemState(tree = tree.move(entry, parent))
      case _                                           => FilesystemState.empty
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
  def empty: FilesystemState = FilesystemState(ReplicatedTree.empty[FsEntry])

  given Lattice[FilesystemState] = Lattice.derived
  given Bottom[FilesystemState]  = Bottom.provide(FilesystemState(ReplicatedTree.empty[FsEntry]))
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

    val markEntrySelectedEvent  = Evt[Dot]()
    val setEntryAsLocationEvent = Evt[Dot]()
    val moveEntryToParentEvent  = Evt[MoveEntry]()
    val renameEntryEvent        = Evt[(Dot, String)]()

    def events[T](s: Seq[FsEntryView], mapper: FsEntryView => Event[T]): Event[T] = {
      val events = s.map(mapper)

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
          Fold(init)(
            FilesystemDataManager.resetBuffer,
            addRandomFileButton.event.branch { _ =>
              val name = scala.util.Random.alphanumeric.take(8).mkString
              current.mod((s) => s.addEntry(s.location, dot => FsEntry.file(dot, name)))
            },
            addRandomFolderButton.event.branch { _ =>
              val name = scala.util.Random.alphanumeric.take(8).mkString
              current.mod((s) => s.addEntry(s.location, dot => FsEntry.folder(dot, name)))
            },
            deleteAllButton.event.branch { _ => current.mod(_.clearAll()) },
            markEntrySelectedEvent.branch { id =>
              current.mod((s) => s.markSelected(id))
            },
            setEntryAsLocationEvent.branch { id =>
              current.mod((s) => s.setLocation(id))
            },
            moveEntryToParentEvent.branch { op =>
              current.mod(_.moveToParent(op.id, op.parent))
            },
            renameEntryEvent.branch { (id, v) =>
              current.mod(_.renameEntry(id, v))
            },
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
              if current.state.location == ReplicatedTree.rootDot then current
              else {
                val node = current.state.tree.node(current.state.location).get
                current.modUntracked(_.setLocation(node.parent))
              }
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

    val parent: Signal[Option[ReplicatedTree.Node[FsEntry]]] = state.map { s =>
      s.tree.node(s.location)
    }

    val entryNodes: Signal[List[ReplicatedTree.Node[FsEntry]]] =
      state.map((s) => s.tree.children(s.location).toList.sorted(using FsEntry.lexicographicOrdering))

    val entryViews: Signal[Seq[FsEntryView]] = entryNodes.map { entries =>
      entries.map((n) => FsEntryView(n.value))
    }

    val e1 = entryViews.map { views =>
      views.map(v => v.onDoubleClick.event.filter(_ => v.state.ty == FsEntryType.Folder).map(_ => v.state.id))
    }.flatten(using Flatten.firstFiringEvent)
    e1.observe(d => setEntryAsLocationEvent.fire(d))
    val e2 = entryViews.map { views =>
      views.map(v => v.onClick.event.map(_ => v.state.id))
    }.flatten(using Flatten.firstFiringEvent)
    e2.observe(d => markEntrySelectedEvent.fire(d))
    val e3 = entryViews.map { views =>
      views.map(v => v.onEntryDrop)
    }.flatten(using Flatten.firstFiringEvent)
    e3.observe(d => moveEntryToParentEvent.fire(d))
    val e4 = entryViews.map { views =>
      views.map(v => v.onRename.map(name => (v.state.id, name)))
    }.flatten(using Flatten.firstFiringEvent)
    e4.observe(d => renameEntryEvent.fire(d))

    val entryTags: Signal[Seq[LI]] = entryViews.map { entries =>
      entries.map((n) => {
        n.toTag(state.map(_.isSelected(n.state.id)))
      })
    }

    val entryList =
      ul(`class` := "filesystem-list").render.reattach(entryTags)

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
        goToParent.data.render.reattach(
          DomHelper.enabledWhen(state.map(_.location != ReplicatedTree.rootDot))
        ),
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
