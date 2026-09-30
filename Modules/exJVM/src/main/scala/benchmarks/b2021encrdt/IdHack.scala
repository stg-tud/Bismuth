package benchmarks.b2021encrdt

import rdts.base.{ReplicaId, Uid}

given idFromString: Conversion[String, Uid]           = rdts.base.Uid.predefined
given localidFromString: Conversion[String, ReplicaId] = rdts.base.ReplicaId.predefined
