package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.IdentityFactory
import ex2024travel.lofi_acl.sync.QueueAppendingMessageReceiver
import ex2024travel.lofi_acl.sync.bft.ReplicaWithBftAclTest.peek
import munit.FunSuite
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.filters.Filter

import java.lang.reflect.Field

class ReplicaWithBftAclTest extends FunSuite {

  override def munitIgnore: Boolean = isCI

  type LWW = LastWriterWins[String]
  given Bottom[String]      = Bottom.provide("")
  given JsonValueCodec[LWW] = JsonCodecMaker.make
  given Filter[LWW]         = Filter.terminalLwwFilter

  private val privateIdA = IdentityFactory.createNewIdentity
  private val privateIdB = IdentityFactory.createNewIdentity
  private val privateIdC = IdentityFactory.createNewIdentity
  private val privateIdD = IdentityFactory.createNewIdentity

  private val aclRoot = BftAclOpGraph.createSelfSignedRoot(privateIdA)

  test("connection works") {
    val rcvA = QueueAppendingMessageReceiver()
    val rcvB = QueueAppendingMessageReceiver()

    val replicaA = ReplicaWithBftAcl[LWW](privateIdA, aclRoot, delta => ())
    replicaA.start()
    val replicaB = ReplicaWithBftAcl[LWW](privateIdB, aclRoot, delta => ())
    replicaB.start()
    val replicaC = ReplicaWithBftAcl[LWW](privateIdC, aclRoot, delta => ())
    replicaC.start()
    val replicaD = ReplicaWithBftAcl[LWW](privateIdD, aclRoot, delta => ())
    replicaD.start()

    replicaA.connect(privateIdB.getPublic, replicaB.address)
    replicaB.connect(privateIdB.getPublic, replicaC.address)
    replicaC.connect(privateIdB.getPublic, replicaD.address)

    val antiEntropyField = replicaA.getClass.getDeclaredField("antiEntropy")
    type T = BftFilteringAntiEntropy[LWW]
    val antiEntropyA = peek[T](antiEntropyField, replicaA)
    val antiEntropyB = peek[T](antiEntropyField, replicaB)
    val antiEntropyC = peek[T](antiEntropyField, replicaC)
    val antiEntropyD = peek[T](antiEntropyField, replicaD)

    Thread.sleep(1000)

    val allIds = Set(privateIdA.getPublic, privateIdB.getPublic, privateIdC.getPublic, privateIdD.getPublic)
    assertEquals(antiEntropyA.connectedPeers, allIds - privateIdA.getPublic)
    assertEquals(antiEntropyB.connectedPeers, allIds - privateIdB.getPublic)
    assertEquals(antiEntropyC.connectedPeers, allIds - privateIdC.getPublic)

    replicaA.currentState
  }
}

object ReplicaWithBftAclTest {
  private def peek[T](field: Field, obj: AnyRef): T = {
    field.setAccessible(true)
    val value = field.get(obj)
    value.asInstanceOf[T]
  }
}
