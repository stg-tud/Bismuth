package text_rdt

given canEqualNullableNull[T]: CanEqual[T | Null, Null] = CanEqual.derived
given canEqualNullNullable[T]: CanEqual[Null, T | Null] = CanEqual.derived

object Helper {

  final val ENABLE =
    !sys.props.get("textrdt.assertions").contains("disabled")

  def myAssert(
      assertion: => Boolean,
      message: => Any
  ): Unit = {
    if (ENABLE) {
      if (!assertion) {
        throw new java.lang.AssertionError("assertion failed: " + message)
      }
    }
  }

  def myAssert(assertion: => Boolean): Unit = {
    if (ENABLE) {
      if (!assertion) {
        throw new java.lang.AssertionError("assertion failed")
      }
    }
  }
}
