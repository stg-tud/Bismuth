package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedTree

import scala.language.implicitConversions
import rdts.time.Dot
import rdts.base.Lattice.assertEquals
import webapps.filesystem.FilesystemState
import rdts.experiments.UndoRedoReplica
import webapps.filesystem.FsEntry
import java.nio.file.Files
import scala.util.Random

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

    sut = sut.mod(_.addEntry(sut.state.location, d => FsEntry.file(d, "file1.txt")))

    {
      assertEquals(sut.state.tree.size, 1)
      assertEquals(childValues(sut.state, sut.state.location), List("file1.txt"))
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
      assertEquals(childValues(sut.state, sut.state.location), List("file1.txt"))

      assert(sut.canUndo)
      assert(!sut.canRedo)
    }
  }

  test("move file entry") {
    given LocalUid = LocalUid(Uid.predefined("A"))

    var sut = UndoRedoReplica.empty[FilesystemState]

    sut = sut.mod(_.addEntry(sut.state.location, d => FsEntry.folder(d, "folder")))
    sut = sut.mod(_.addEntry(sut.state.location, d => FsEntry.file(d, "file1.txt")))

    val folderDot = sut.state.tree.children(sut.state.location).find(_.value.name.value == "folder").get.dot
    val fileDot   = sut.state.tree.children(sut.state.location).find(_.value.name.value == "file1.txt").get.dot

    {
      val state = sut.state
      assertEquals(state.tree.size, 2)
      assertEquals(childValues(state, state.location), List("file1.txt", "folder"))
    }

    sut = sut.mod(_.moveToParent(fileDot, folderDot))

    {
      val state = sut.state
      assertEquals(state.tree.size, 2)
      assertEquals(childValues(state, state.location), List("folder"))
      assertEquals(childValues(state, folderDot), List("file1.txt"))
      assert(sut.canUndo)
      assert(!sut.canRedo)
    }

    sut = sut.undo()

    {
      val state = sut.state
      assertEquals(state.tree.size, 2)
      assertEquals(childValues(state, state.location), List("file1.txt", "folder"))
    }

    sut = sut.undo()

    {
      val state = sut.state
      assertEquals(state.tree.size, 1)
      assertEquals(childValues(state, state.location), List("folder"))
    }

    sut = sut.undo()

    {
      assertEquals(sut.state.tree.size, 0)
    }

    sut = sut.redo()

    {
      val state = sut.state
      assertEquals(state.tree.size, 1)
      assertEquals(childValues(state, state.location), List("folder"))
    }

    sut = sut.redo()

    {
      val state = sut.state
      assertEquals(state.tree.size, 2)
      assertEquals(childValues(state, state.location), List("file1.txt", "folder"))
    }

    sut = sut.redo()

    {
      val state = sut.state

      assertEquals(state.tree.size, 2)
      assertEquals(childValues(state, state.location), List("folder"))
      assertEquals(childValues(state, folderDot), List("file1.txt"))
      assert(sut.canUndo)
      assert(!sut.canRedo)
    }
  }
}

def childValues(state: FilesystemState, parent: Dot): List[String] = {
  state.tree.children(parent).toList.sorted(using FsEntry.lexicographicOrdering).map(_.value.name.value).toList
}
