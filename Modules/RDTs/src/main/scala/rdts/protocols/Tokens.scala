package rdts.protocols

import rdts.base.{Bottom, Lattice, ReplicaId, Orderings, Uid}
import rdts.datatypes.ReplicatedSet

case class Ownership(epoch: Long, owner: Uid)

object Ownership {
  given Lattice[Ownership] = Lattice.fromOrdering(using Orderings.lexicographic)

  given bottom: Bottom[Ownership] = Bottom.provide(Ownership(Long.MinValue, Uid.zero))

  def unchanged: Ownership = bottom.empty
}

case class Token(os: Ownership, wants: ReplicatedSet[Uid]) {

  def isOwner(using replicaId: ReplicaId): Boolean = replicaId.uid == os.owner

  def request(using replicaId: ReplicaId): Token =
    Token(Ownership.unchanged, wants.add(replicaId.uid))

  def release(using replicaId: ReplicaId): Token =
    Token(Ownership.unchanged, wants.remove(replicaId.uid))

  def upkeep(using ReplicaId): Token =
    if !isOwner then Token.unchanged
    else
        selectFrom(wants) match
            case None            => Token.unchanged
            case Some(nextOwner) =>
              Token(Ownership(os.epoch + 1, nextOwner), ReplicatedSet.empty)

  def selectFrom(wants: ReplicatedSet[Uid])(using replicaId: ReplicaId): Option[Uid] =
    // We find the “largest” ID that wants the token.
    // This is incredibly “unfair” but does prevent deadlocks in case someone needs multiple tokens.
    wants.elements.maxOption.filter(id => id != replicaId.uid)

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
  def transform(f: T => T)(using ReplicaId): T =
    if token.isOwner then f(value) else Bottom.empty
}
