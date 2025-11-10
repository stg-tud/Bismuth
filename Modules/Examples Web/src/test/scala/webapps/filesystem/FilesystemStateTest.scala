package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedTree

import scala.language.implicitConversions
import rdts.time.Dot
import rdts.base.Lattice.assertEquals
import webapps.filesystem.FilesystemState
import rdts.experiments.UndoRedoReplica
import webapps.filesystem.FsEntryView

class FilesystemStateTest extends munit.FunSuite {
  type SUT = UndoRedoReplica[FilesystemState]

  test("empty") {
    given LocalUid = LocalUid(Uid.predefined("A"))

    val sut = UndoRedoReplica.empty[FilesystemState]

    assertEquals(sut.state.selection, ReplicatedTree.rootDot)
    assertEquals(sut.state.location, ReplicatedTree.rootDot)
    assertEquals(sut.state.tree.size, 0)
  }

  test("add file entry") {
    given LocalUid = LocalUid(Uid.predefined("A"))

    var sut = UndoRedoReplica.empty[FilesystemState]

    {
      assertEquals(sut.state.selection, ReplicatedTree.rootDot)
      assertEquals(sut.state.location, ReplicatedTree.rootDot)
      assertEquals(sut.state.tree.size, 0)
    }

    sut = sut.mod(_.addEntry(sut.state.location, d => FsEntryView.file(d, "file1.txt")))

    {
      assertEquals(sut.state.tree.size, 1)
      val children = sut.state.tree.children(sut.state.location).toList
      assertEquals(children.size, 1)
      val entry = children.head
      assertEquals(entry.value.name.value, "file1.txt")
      assert(sut.canUndo)
      assert(!sut.canRedo)
    }

    sut = sut.undo()

    {
      assert(!sut.canUndo)
      assert(sut.canRedo)
      assertEquals(sut.state.tree.size, 0)
    }

    sut = sut.redo()

    {
      assertEquals(sut.state.tree.size, 1)
      val children = sut.state.tree.children(sut.state.location).toList
      assertEquals(children.size, 1)
      val entry = children.head
      assertEquals(entry.value.name.value, "file1.txt")
      assert(sut.canUndo)
      assert(!sut.canRedo)
    }
  }
}
