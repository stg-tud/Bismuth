package benchmarks.taskapp

import rdts.time.Dot
import rdts.datatypes.ReplicatedTree
import rdts.datatypes.LastWriterWins as LWW
import benchmarks.taskapp.TaskApp.{Entry, Task}

import scala.collection.mutable
import scala.util.Random

/** Generates interactions on-the-fly based on actual app state.
  * This ensures the Dots used in interactions match the real app's Dots.
  */
class TaskAppInteractionGenerator(
    val pruningThreshold: Int = 50,
    val keptTasks: Int = 20,
    val maxFolders: Int = 100,
    val maxTaskLists: Int = 200,
    val textLengthMin: Int = 10,
    val textLengthMax: Int = 100,
    seed: Long = 42
) {
  private val random = new Random(seed)

  /** Generate the next interaction based on actual tree state */
  def nextInteraction(tree: ReplicatedTree[Entry]): TaskAppInteraction = {
    val (folders, taskLists, totalTaskCount) = analyzeTree(tree)

    // Pruning: if we have too many tasks, remove some
    if totalTaskCount >= pruningThreshold then {
      removeRandomTask(taskLists).getOrElse(performRandomInteraction(tree, folders, taskLists, totalTaskCount))
    } else {
      performRandomInteraction(tree, folders, taskLists, totalTaskCount)
    }
  }

  /** First interaction should always create a task list */
  def initialInteraction(): TaskAppInteraction =
    AddTaskList(ReplicatedTree.rootDot, s"List_${randomString(5, 15)}")

  private def analyzeTree(tree: ReplicatedTree[Entry]): (Seq[Dot], Map[Dot, Int], Int) = {
    val folders    = mutable.ArrayBuffer[Dot]()
    val taskLists  = mutable.Map[Dot, Int]() // dot -> task count
    var totalTasks = 0

    tree.nodes.foreach { node =>
      node.value match {
        case Entry.FolderEntry(_) =>
          folders += node.dot
        case Entry.TaskListEntry(list) =>
          val taskCount = list.items.toList.size
          taskLists(node.dot) = taskCount
          totalTasks += taskCount
      }
    }

    (folders.toSeq, taskLists.toMap, totalTasks)
  }

  private def performRandomInteraction(
      tree: ReplicatedTree[Entry],
      folders: Seq[Dot],
      taskLists: Map[Dot, Int],
      totalTaskCount: Int
  ): TaskAppInteraction = {
    // Weights for different interactions:
    // - 10% add folder (if under limit)
    // - 15% add task list (if under limit)
    // - 30% add task to a list
    // - 5% move entry
    // - 5% update folder name
    // - 5% update task list name
    // - 5% remove task
    // - 5% move task within list
    // - 5% update task title
    // - 5% update task description

    val choice = random.nextInt(100)

    if choice < 10 then {
      if folders.size < maxFolders then addFolder(folders)
      else updateFolderName(folders).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else if choice < 25 then {
      if taskLists.size < maxTaskLists then addTaskList(folders)
      else updateTaskListName(taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else if choice < 55 then {
      addTask(taskLists).getOrElse(addTaskList(folders))
    } else if choice < 60 then {
      moveEntry(folders, taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else if choice < 65 then {
      updateFolderName(folders).getOrElse(addFolder(folders))
    } else if choice < 70 then {
      updateTaskListName(taskLists).getOrElse(addTaskList(folders))
    } else if choice < 75 then {
      removeRandomTask(taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else if choice < 80 then {
      moveTaskWithinList(taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else if choice < 85 then {
      updateTaskTitle(taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    } else {
      updateTaskDescription(taskLists).getOrElse(addTask(taskLists).getOrElse(addTaskList(folders)))
    }
  }

  private def randomString(minLen: Int, maxLen: Int): String = {
    val length = minLen + random.nextInt(maxLen - minLen + 1)
    val chars  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    val sb     = new StringBuilder(length)
    var i      = 0
    while i < length do {
      sb.append(chars.charAt(random.nextInt(chars.length)))
      i += 1
    }
    sb.toString
  }

  private def getRandomFolder(folders: Seq[Dot]): Dot = {
    if folders.isEmpty || random.nextBoolean() then ReplicatedTree.rootDot
    else folders(random.nextInt(folders.size))
  }

  private def addFolder(folders: Seq[Dot]): AddFolder = {
    val parentDot = getRandomFolder(folders)
    val name      = s"Folder_${randomString(5, 15)}"
    AddFolder(parentDot, name)
  }

  private def addTaskList(folders: Seq[Dot]): AddTaskList = {
    val parentDot = getRandomFolder(folders)
    val name      = s"List_${randomString(5, 15)}"
    AddTaskList(parentDot, name)
  }

  private def addTask(taskLists: Map[Dot, Int]): Option[AddTask] = {
    if taskLists.isEmpty then return None

    val taskListDots = taskLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))

    val title       = s"Task_${randomString(5, 20)}"
    val description = if random.nextBoolean() then Some(randomString(10, 50)) else None

    Some(AddTask(
      taskListDot,
      Task(
        title = LWW.now(title),
        description = LWW.now(description)
      )
    ))
  }

  private def moveEntry(folders: Seq[Dot], taskLists: Map[Dot, Int]): Option[MoveEntry] = {
    val entries = folders ++ taskLists.keys.toSeq
    if entries.size < 2 then return None

    // Pick a random entry to move
    val entryToMove = entries(random.nextInt(entries.size))

    // Pick a random folder as the new parent (excluding moving to self)
    val possibleParents = (Seq(ReplicatedTree.rootDot) ++ folders).filterNot(_ == entryToMove)
    if possibleParents.isEmpty then return None

    val newParent = possibleParents(random.nextInt(possibleParents.size))
    Some(MoveEntry(entryToMove, newParent))
  }

  private def updateFolderName(folders: Seq[Dot]): Option[UpdateFolderName] = {
    if folders.isEmpty then return None

    val folderDot = folders(random.nextInt(folders.size))
    val newName   = s"Folder_${randomString(5, 15)}"
    Some(UpdateFolderName(folderDot, newName))
  }

  private def updateTaskListName(taskLists: Map[Dot, Int]): Option[UpdateTaskListName] = {
    if taskLists.isEmpty then return None

    val taskListDots = taskLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))
    val newName      = s"List_${randomString(5, 15)}"
    Some(UpdateTaskListName(taskListDot, newName))
  }

  private def removeRandomTask(taskLists: Map[Dot, Int]): Option[RemoveTaskListItem] = {
    val nonEmptyLists = taskLists.filter(_._2 > 0)
    if nonEmptyLists.isEmpty then return None

    val taskListDots = nonEmptyLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))
    val taskCount    = nonEmptyLists(taskListDot)
    val taskIndex    = random.nextInt(taskCount)

    Some(RemoveTaskListItem(taskListDot, taskIndex))
  }

  private def moveTaskWithinList(taskLists: Map[Dot, Int]): Option[MoveTaskListItem] = {
    // Find a task list with at least 2 tasks
    val eligibleLists = taskLists.filter(_._2 >= 2)
    if eligibleLists.isEmpty then return None

    val taskListDots = eligibleLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))
    val taskCount    = eligibleLists(taskListDot)

    val fromIndex = random.nextInt(taskCount)
    var toIndex   = random.nextInt(taskCount)
    while toIndex == fromIndex do
        toIndex = random.nextInt(taskCount)

    Some(MoveTaskListItem(taskListDot, fromIndex, toIndex))
  }

  private def updateTaskTitle(taskLists: Map[Dot, Int]): Option[UpdateTaskTitle] = {
    val nonEmptyLists = taskLists.filter(_._2 > 0)
    if nonEmptyLists.isEmpty then return None

    val taskListDots = nonEmptyLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))
    val taskCount    = nonEmptyLists(taskListDot)
    val taskIndex    = random.nextInt(taskCount)

    val newTitle = s"Task_${randomString(5, 20)}"
    Some(UpdateTaskTitle(taskListDot, taskIndex, newTitle))
  }

  private def updateTaskDescription(taskLists: Map[Dot, Int]): Option[UpdateTaskDescription] = {
    val nonEmptyLists = taskLists.filter(_._2 > 0)
    if nonEmptyLists.isEmpty then return None

    val taskListDots = nonEmptyLists.keys.toSeq
    val taskListDot  = taskListDots(random.nextInt(taskListDots.size))
    val taskCount    = nonEmptyLists(taskListDot)
    val taskIndex    = random.nextInt(taskCount)

    val newDescription = if random.nextBoolean() then Some(randomString(10, 50)) else None
    Some(UpdateTaskDescription(taskListDot, taskIndex, newDescription))
  }
}
