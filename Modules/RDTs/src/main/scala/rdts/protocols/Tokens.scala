package rdts.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Orderings, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.time.Dots

case class Ownership(epoch: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(using Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {

  def isOwner(using LocalUid): Boolean = replicaId == os.owner

  def request(using LocalUid): Token =
    Token(Ownership.unchanged, wants.add(replicaId))

  def release(using LocalUid): Token =
    Token(Ownership.unchanged, wants.remove(replicaId))

  def upkeep(using LocalUid): Token =
    if !isOwner then Token.unchanged
    else
      selectFrom(wants) match
        case None            => Token.unchanged
        case Some(nextOwner) =>
          Token(Ownership(os.epoch + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using LocalUid) =
    // We find the “largest” ID that wants the token.
    // This is incredibly “unfair” but does prevent deadlocks in case someone needs multiple tokens.
    wants.elements.maxOption.filter(id => id != replicaId)

}

object Token {
  val unchanged: Token = Token(Ownership.unchanged, ReplicatedSet.empty)
  given Lattice[Token] = Lattice.derived
}

case class ExampleTokens(
    calendarAinteractionA: Token,
    calendarBinteractionA: Token
)

case class Exclusive[T: {Bottom}](token: Token, value: T) {
  def transform(f: T => T)(using LocalUid) =
    if token.isOwner then f(value) else Bottom.empty
}
