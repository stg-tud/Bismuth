package benchmarks.taskapp

import benchmarks.taskapp.TaskApp.Task
import rdts.time.Dot

sealed trait TaskAppInteraction

case class AddTaskList(parentFolder: Dot, name: String) extends TaskAppInteraction

case class AddFolder(parentFolder: Dot, name: String) extends TaskAppInteraction

case class MoveEntry(entryId: Dot, newParent: Dot) extends TaskAppInteraction

case class RemoveEntry(entryId: Dot) extends TaskAppInteraction

case class UpdateFolderName(folder: Dot, newName: String) extends TaskAppInteraction

case class UpdateTaskListName(taskListId: Dot, newName: String) extends TaskAppInteraction

case class AddTask(taskListId: Dot, task: Task) extends TaskAppInteraction

case class RemoveTaskListItem(taskListId: Dot, itemIx: Int) extends TaskAppInteraction

case class MoveTaskListItem(taskListId: Dot, from: Int, to: Int) extends TaskAppInteraction

case class UpdateTaskTitle(taskListId: Dot, itemIx: Int, newTitle: String) extends TaskAppInteraction

case class UpdateTaskDone(taskListId: Dot, itemIx: Int) extends TaskAppInteraction

case class UpdateTaskDescription(taskListId: Dot, itemIx: Int, newDescription: Option[String])
    extends TaskAppInteraction

case class MarkItemsAsDoneThatMatch(taskListId: Dot, text: String) extends TaskAppInteraction

case object Undo extends TaskAppInteraction

case object Redo extends TaskAppInteraction
