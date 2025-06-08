package reactives.core

import reactives.macros.Sourcecode

/** Provides names for dynamic dependencies based on their definition position to allow easier debugging */
case class ReInfo(idCounter: Int, description: String, enclosing: String, file: String, line: Int) {
  def derive(derivation: String): ReInfo = copy(description = s"»$description«'$derivation")
}

object ReInfo {

  def apply(name: String)(using info: ReInfo): ReInfo = info.derive(name)

  def named[T](name: String)(f: ReInfo ?=> T)(using info: ReInfo) = f(using info.derive(name))

  given create(using
      file: Sourcecode.File,
      enclosing: Sourcecode.Enclosing,
      line: Sourcecode.Line
  ): ReInfo = ReInfo(nextCount(), "", enclosing.value.intern(), file.value.intern(), line.value)

  private var counter: Int = 0
  private def nextCount()  = synchronized {
    counter = counter + 1
    counter
  }

}
