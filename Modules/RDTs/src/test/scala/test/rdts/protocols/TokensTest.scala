package test.rdts.protocols

import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.{Ownership, Token}
import rdts.time.Dots

class TokensTest extends munit.FunSuite {
  given dots: Dots            = Dots.empty
  given Lattice[Token]        = Lattice.derived
  val numOfReplicas           = 5
  val replicas: Seq[LocalUid] = List.tabulate(numOfReplicas)(_ => LocalUid.gen())
  var token: Token            = Token(
    os = Ownership.unchanged,
    wants = ReplicatedSet.empty
  )
  // set replica 0 the initial owner
  token = token.merge(Token(Ownership(1, LocalUid.unwrap(replicas(0))), ReplicatedSet.empty))

  test("Some replica initially owns the token") {
    assert(List.range(0, numOfReplicas).map(n => token.isOwner(using replicas(n))).reduce((x, y) => x || y))
  }
  test("Owner doesn't change if no replica wants the token") {
    // replica 0, the current owner, calls upkeep
    val updatedToken = token.merge(token.upkeep(using replicas(0)))
    // wants is empty, owner should remain same
    assert(updatedToken.isOwner(using replicas(0)))
  }
  test("Interested replica with the biggest id gets the token next") {
    // replicas 2 and 3 request token
    var updatedToken = token.merge(token.request(using replicas(1)))
    updatedToken = updatedToken.merge(updatedToken.request(using replicas(2)))
    // find biggest id in wants
    val biggestIdIndex = replicas.indexOf(LocalUid(updatedToken.wants.elements.max))
    // replica 0, the current owner, calls upkeep
    updatedToken = updatedToken.merge(updatedToken.upkeep(using replicas(0)))
    // assert that the new owner is the one with the biggest id
    assert(updatedToken.isOwner(using replicas(biggestIdIndex)))
  }
  test("Replica that isn't the owner can't change owner") {
    // replica 1 is not the owner and calls upkeep
    val updatedToken = token.merge(token.upkeep(using replicas(1)))
    assert(updatedToken.isOwner(using replicas(0)))
  }
  test("Owner can't choose itself as the next owner if other replicas interested") {
    // replica 1 interested
    var updatedToken = token.merge(token.request(using replicas(1)))
    // replica 0 calls upkeep, 1 should be the new owner
    updatedToken = updatedToken.merge(updatedToken.upkeep(using replicas(0)))
    // assert that replica 0 is not the new owner
    assert(!updatedToken.isOwner(using replicas(0)))
  }
  test("Replica that never requested can't be chosen to be the owner") {
    // replicas 1 & 2 request the token
    var updatedToken = token.merge(token.request(using replicas(1)))
    updatedToken = updatedToken.merge(token.request(using replicas(2)))
    // replica 0 calls upkeep
    updatedToken = updatedToken.merge(updatedToken.upkeep(using replicas(0)))
    // assert neither replica 3 nor 4 were chosen to be the owner
    assert(!updatedToken.isOwner(using replicas(3)) &&
      !updatedToken.isOwner(using replicas(4)))
  }
}
