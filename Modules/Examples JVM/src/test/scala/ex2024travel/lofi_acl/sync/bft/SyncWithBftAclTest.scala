package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.IdentityFactory
import ex2024travel.lofi_acl.sync.QueueAppendingMessageReceiver
import ex2024travel.lofi_acl.sync.bft.SyncWithBftAclTest.peek
import munit.FunSuite
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.filters.Filter

import java.lang.reflect.Field

class SyncWithBftAclTest extends FunSuite {

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

    val syncA = SyncWithBftAcl[LWW](privateIdA, aclRoot, delta => ())
    syncA.start()
    val syncB = SyncWithBftAcl[LWW](privateIdB, aclRoot, delta => ())
    syncB.start()
    val syncC = SyncWithBftAcl[LWW](privateIdC, aclRoot, delta => ())
    syncC.start()
    val syncD = SyncWithBftAcl[LWW](privateIdD, aclRoot, delta => ())
    syncD.start()

    syncA.connect(privateIdB.getPublic, syncB.address)
    syncB.connect(privateIdB.getPublic, syncC.address)
    syncC.connect(privateIdB.getPublic, syncD.address)

    val antiEntropyField = syncA.getClass.getDeclaredField("antiEntropy")
    type T = BftFilteringAntiEntropy[LWW]
    val antiEntropyA = peek[T](antiEntropyField, syncA)
    val antiEntropyB = peek[T](antiEntropyField, syncB)
    val antiEntropyC = peek[T](antiEntropyField, syncC)
    val antiEntropyD = peek[T](antiEntropyField, syncD)

    Thread.sleep(1000)

    val allIds = Set(privateIdA.getPublic, privateIdB.getPublic, privateIdC.getPublic, privateIdD.getPublic)
    assertEquals(antiEntropyA.connectedPeers, allIds - privateIdA.getPublic)
    assertEquals(antiEntropyB.connectedPeers, allIds - privateIdB.getPublic)
    assertEquals(antiEntropyC.connectedPeers, allIds - privateIdC.getPublic)

    syncA.currentState
  }
}

object SyncWithBftAclTest {
  private def peek[T](field: Field, obj: AnyRef): T = {
    field.setAccessible(true)
    val value = field.get(obj)
    value.asInstanceOf[T]
  }
}
