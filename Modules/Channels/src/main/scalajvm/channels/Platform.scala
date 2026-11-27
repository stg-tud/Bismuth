package channels

/** Compile time platform constants. Branches on these constants (`if Platform.isJvm then …`) are folded by the compiler: all
  * platforms type check against the JDK class library, and the eliminated branch is never linked.
  */
object Platform {
  inline val isJvm    = true
  inline val isJs     = false
  inline val isNative = false
}
