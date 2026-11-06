package text_rdt

object Helper {

  final val ENABLE =
    !sys.props.get("textrdt.assertions").contains("disabled")

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
