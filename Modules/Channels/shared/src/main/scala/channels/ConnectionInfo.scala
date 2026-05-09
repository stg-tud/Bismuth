package channels

object ConnectionInfo {
  def apply(details: (String, String)*): ConnectionInfo = ConnectionInfo(details = details.toMap)

  def structured(
      local: Option[ConnectionDescriptor] = None,
      remote: Option[ConnectionDescriptor] = None,
      details: (String, String)*
  ): ConnectionInfo =
    ConnectionInfo(local = local, remote = remote, details = details.toMap)
}

case class ConnectionInfo(
    local: Option[ConnectionDescriptor] = None,
    remote: Option[ConnectionDescriptor] = None,
    details: Map[String, String] = Map.empty,
)
