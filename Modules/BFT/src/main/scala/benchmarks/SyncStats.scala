package benchmarks

class SyncStats:
    @volatile private var messageCounter = 0
    @volatile private var bandwidth      = 0

    def incMessageCounter(): Unit  = synchronized { messageCounter += 1 }
    def addBandwidth(n: Int): Unit = synchronized { bandwidth += n }

    def results: (Int, Int) = (messageCounter, bandwidth)
