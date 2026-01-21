package benchmarks

import datatypes.{Counter, ORSet}

object ReplicaExamples:
    object Example1:
        var replica1 = ORSet[String]()
        var replica2 = ORSet[String]()
        var replica3 = ORSet[String]()
        var replica4 = ORSet[String]()

        // A
        replica1 = replica1.merge(replica1.add("A"))
        // B
        replica1 = replica1.merge(replica1.add("B"))

        replica2 = replica2.merge(replica1)
        replica3 = replica3.merge(replica1)
        replica4 = replica4.merge(replica1)

        // C
        replica1 = replica1.merge(replica1.add("C"))

        // D
        replica1 = replica1.merge(replica1.add("D"))

        // J
        replica3 = replica3.merge(replica3.add("J"))

        // K
        replica3 = replica3.merge(replica3.add("K"))

        // F
        replica2 = replica2.merge(replica2.add("F"))

        // G
        replica2 = replica2.merge(replica2.add("G"))

        replica1 = replica1.merge(replica3)
        replica2 = replica2.merge(replica3)

        // E
        replica1 = replica1.merge(replica1.add("E"))

        // L
        replica3 = replica3.merge(replica3.add("L"))

        // M
        replica3 = replica3.merge(replica3.add("M"))

        replica1 = replica1.merge(replica3)

    object Example2:
        var replica1: ORSet[String] = Example1.replica1
        var replica2: ORSet[String] = Example1.replica2

        for i <- Range(0, 1000) do
            replica1 = replica1.merge(replica1.add(i.toString))
            replica2 = replica2.merge(replica2.add(i.toString))

    object Example3:
        var replica1: ORSet[String] = Example1.replica1
        var replica2: ORSet[String] = Example1.replica2

        for i <- Range(0, 200) do
            replica1 = replica1.merge(replica1.add(i.toString))
            replica2 = replica2.merge(replica2.add(i.toString))

        replica1 = replica1.merge(replica2)
        replica2 = replica2.merge(replica1)

        for i <- Range(0, 100) do
            replica1 = replica1.merge(replica1.add(i.toString))
            replica2 = replica2.merge(replica2.add(i.toString))

    object Example4:
        var replica1: Counter = Counter()
        var replica2: Counter = Counter()

        for i <- Range(0, 500) do
            replica1 = replica1.merge(replica1.inc)
            replica2 = replica2.merge(replica2.inc)

        replica1 = replica1.merge(replica2)
        replica2 = replica2.merge(replica1)

        for i <- Range(0, 500) do
            replica1 = replica1.merge(replica1.add(10))
            replica2 = replica2.merge(replica2.add(5))
