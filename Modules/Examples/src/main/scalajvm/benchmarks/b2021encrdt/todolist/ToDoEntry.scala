package benchmarks.b2021encrdt.todolist

import java.time.LocalDateTime

case class ToDoEntry(text: String, completed: Boolean, timeAdded: LocalDateTime) {}
