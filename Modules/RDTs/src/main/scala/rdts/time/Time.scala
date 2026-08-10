package rdts.time

type Time = Long

object Time:
    /* System.nanoTime is guaranteed to be monotonic even across threads.
     * However it’s not comparable across different JVM or JVM restarts,
     * thus, we use the anchors below, to roughly anchor the nanotime and the current milliseconds.
     */
    private val anchorMillis: Long = System.currentTimeMillis()
    private val anchorNanos: Long  = System.nanoTime()

    def wallClockNanos(): Time = anchorMillis * 1_000_000L + (System.nanoTime() - anchorNanos)
