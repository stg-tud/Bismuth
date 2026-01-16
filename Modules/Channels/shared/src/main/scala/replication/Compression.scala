package replication

import java.io.ByteArrayOutputStream
import java.util.zip.{DeflaterOutputStream, InflaterOutputStream}

object Compression {
  def compress(in: Array[Byte]): Array[Byte] =
      val out = new ByteArrayOutputStream
      val defl = new DeflaterOutputStream(out)
      defl.write(in)
      defl.flush()
      defl.finish()
      defl.close()
      out.toByteArray

  def decompress(in: Array[Byte]): Array[Byte] =
      val out  = new ByteArrayOutputStream
      val infl = new InflaterOutputStream(out)
      infl.write(in)
      infl.flush()
      infl.finish()
      infl.close()
      out.toByteArray
}
