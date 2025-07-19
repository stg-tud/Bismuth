package rdts.filters

case class InvalidPathException(path: List[String]) extends RuntimeException {
  override def toString: String = s"InvalidPathException: ${path.mkString(".")}"
}
