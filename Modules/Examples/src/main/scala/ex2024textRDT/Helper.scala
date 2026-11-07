package ex2024textRDT

object Helper {

  /** Enables expensive internal consistency checks. Change to `true` locally when debugging the algorithms. */
  final val ENABLE: Boolean = false

  def myAssert(
      assertion: => Boolean,
      message: => Any
  ): Unit =
    if ENABLE then {
      if !assertion then {
        throw new java.lang.AssertionError("assertion failed: " + message)
      }
    }

  def myAssert(assertion: => Boolean): Unit =
    if ENABLE then {
      if !assertion then {
        throw new java.lang.AssertionError("assertion failed")
      }
    }
}
