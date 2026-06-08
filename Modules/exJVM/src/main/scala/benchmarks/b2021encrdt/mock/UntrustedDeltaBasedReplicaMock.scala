package benchmarks.b2021encrdt.mock

import benchmarks.b2021encrdt.Codecs.given
import benchmarks.b2021encrdt.deltabased.{EncryptedDeltaGroup, UntrustedReplica}
import benchmarks.b2021encrdt.{Codecs, localidFromString}
import channels.connection.MessageBuffer
import channels.experiments
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.crypto.tink.Aead
import rdts.syntax.oldCompat.DeltaAWLWWMContainer
import rdts.time.Dots

import java.io.PrintWriter
import java.nio.file.{Files, Path}

class UntrustedDeltaBasedReplicaMock extends UntrustedReplica {
  override protected def prune(encryptedDeltaGroup: EncryptedDeltaGroup): Unit  = {}
  override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {}

  def getCausalContext: Dots = dottedVersionVector

  def size(): Long = {
    encryptedDeltaGroupStore.toList.map { delta =>
      delta.stateCiphertext.remaining().toLong + delta.serialDottedVersionVector.remaining().toLong
    }.sum
  }

  def decryptAndWriteRawDeltasToFile(aead: Aead, outFilepath: Path): Unit = {
    val os          = Files.newOutputStream(outFilepath)
    val printWriter = new PrintWriter(os)
    encryptedDeltaGroupStore.foreach { encDeltaGroup =>
      val vectorbytes = MessageBuffer.convertByteBufferToArray(
        encDeltaGroup.serialDottedVersionVector.duplicate()
      )
      printWriter.print(new String(aead.decrypt(
        MessageBuffer.convertByteBufferToArray(
          encDeltaGroup.stateCiphertext.duplicate()
        ),
        vectorbytes
      )))
      printWriter.print('|')
      printWriter.println(new String(vectorbytes))
    }
    printWriter.close()
  }

  def decryptAndWriteDeltasToFile(aead: experiments.Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    encryptedDeltaGroupStore.foreach(encDeltaGroup => printWriter.println(encDeltaGroup.decrypt[Dots](aead)))
    printWriter.close()
  }

  def decryptAndWriteStateToFile(aead: experiments.Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    val crdt        = decrypt(aead)
    printWriter.write(writeToString(crdt.state)(using Codecs.deltaAwlwwmapJsonCodec))
    printWriter.close()
  }

  def decrypt(aead: experiments.Aead): DeltaAWLWWMContainer[String, String] = {
    val crdt = new DeltaAWLWWMContainer[String, String]("".convert)
    encryptedDeltaGroupStore.map { encDeltaGroup =>
      encDeltaGroup.decrypt(aead)(using Codecs.deltaAwlwwmapJsonCodec)
    }.foreach { decDeltaGroup =>
      crdt.merge(decDeltaGroup.deltaGroup)
    }

    crdt
  }

  def copy(): UntrustedDeltaBasedReplicaMock = {
    val obj = new UntrustedDeltaBasedReplicaMock()
    obj.encryptedDeltaGroupStore = encryptedDeltaGroupStore
    obj.dottedVersionVector = dottedVersionVector
    obj
  }
}
