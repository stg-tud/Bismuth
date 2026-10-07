package rdts.base

import scala.CanEqual
import scala.annotation.implicitNotFound

// opaque currently causes too many weird issues with library integrations, in particular the json libraries can no longer auto serialize
/** Uid’s are serializable abstract unique Ids. Currently implemented as Strings, but subject to change. */
case class Uid(delegate: String) derives CanEqual {
  override def toString: String = show
  def show: String              =
      val offset    = delegate.indexOf('.')
      val shortened = if offset > 0 then delegate.substring(0, offset + 4) else delegate
      s"🪪$shortened"
}

object Uid {
  given ordering: Ordering[Uid]  = Ordering.String.on(_.delegate)
  def predefined(s: String): Uid = Uid(s)
  def unwrap(id: Uid): String    = id.delegate
  val zero: Uid                  = Uid("")

  extension (s: String) def asId: Uid = Uid(s)

  given toLocal: Conversion[Uid, ReplicaId] = x => ReplicaId(x)

  val jvmID: String = UidEncoding.encode(scala.util.Random.nextLong(1L << 48))

  private var idCounter: Long = -1

  /** Generate a new unique ID.
    * Uses 48 bit of a process local random value + up to 64 of a counter.
    * Encoded as a string using 9 bytes + 1 byte per 6 bits of the counter value.
    */
  def gen(): Uid = synchronized {
    idCounter = idCounter + 1

    if idCounter != 0 then Uid(s"${UidEncoding.encode(idCounter)}.$jvmID")
    else Uid(s"$jvmID")
  }

  /** Generate a new ID from 48 bits of `random` alone (no counter), i.e., 8 characters
    * (fewer if the leading bits are zero).
    * Unlike [[gen]], the result is unguessable if `random` is a secure generator (e.g. `java.security.SecureRandom`),
    * so it can be used where knowing an id grants access. Uniqueness is only probabilistic (48 bits).
    */
  def gen(random: java.util.Random): Uid =
    Uid(UidEncoding.encode(random.nextLong() & ((1L << 48) - 1)))
}
@implicitNotFound(
  "Requires a replica ID of the current local replica that is doing the modification."
)
/** Operations may require an ID of the replica doing a modification.
  * We provide it as its own opaque type to make it obvious that this should not be just any ID.
  * Use [[Uid]] if you want to store an ID in a replicated data structure.
  */
case class ReplicaId(uid: Uid) {
  override def toString: String = show
  def show: String              = uid.show
}
object ReplicaId {
  given ordering: Ordering[ReplicaId] = Uid.ordering.on(_.uid)

  extension (s: String) def asId: ReplicaId = predefined(s)

  def predefined(s: String): ReplicaId = Uid.predefined(s).convert
  def unwrap(id: ReplicaId): Uid       = id.uid
  def gen(): ReplicaId                 = Uid.gen().convert
}

object UidEncoding {
  private val alphabet: Array[Char] = Array(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '_'
  )

  private val sb = StringBuilder(12)

  /** Uids are stored as short strings for efficient JSON encoding.
    * The encoding is inspired by 64, but without the additional characters to signify the number of encoded bits.
    * Encodes in chunks of 6 bits, starting from the least significant bits.
    */
  def encode(long: Long): String = synchronized {
    sb.clear()
    var remaining = long
    while remaining != 0 do
        sb.append(alphabet((remaining & 0b111111).toInt))
        remaining = remaining >>> 6
    sb.result()
  }
}
