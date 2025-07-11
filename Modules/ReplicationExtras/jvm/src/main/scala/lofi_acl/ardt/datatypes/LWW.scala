package lofi_acl.ardt.datatypes

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.*
import rdts.filters.Permission.{ALLOW, PARTIAL}
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.filters.{Filter, InvalidPathException, PermissionTree}
import rdts.time.CausalTime
import replication.filters.DeltaSurgeon

type LWW[V] = LastWriterWins[V]

object LWW {


}
