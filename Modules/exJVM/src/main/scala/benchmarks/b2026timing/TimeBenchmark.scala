package benchmarks.b2026timing

import org.openjdk.jmh.annotations.*

import java.time.Instant
import java.util.concurrent.TimeUnit

import rdts.time.Time

/** Compares the cost of the wall-clock helpers in `rdts.time.Time` against
  * `System.nanoTime` and the `java.time.Instant` based alternatives.
  *
  * Clock reads are only tens of nanoseconds, so the returned values keep the
  * calls alive (JMH consumes return values), and the relative ordering between
  * benchmarks is what matters.
  *
  * Precision notes: `System.currentTimeMillis` truncates to milliseconds, and
  * on JDKs before 15 `Instant.now()` was backed by it. Since JDK 15 on Linux,
  * `Instant.now()` / `Clock.systemUTC()` read `CLOCK_REALTIME` via
  * `clock_gettime` and have genuine nanosecond resolution.
  * `Time.wallClockNanos` implements the same resolution portably (also on
  * older JDKs) by anchoring the monotonic `System.nanoTime` to a
  * `currentTimeMillis` base.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms512m", "-Xmx512m"))
class TimeBenchmark {

  // Anchor for the nanosecond-precision wall clock used by `wallClockInstant`:
  // the wall-clock epoch (in nanos) at the moment `anchorNanos` was read is
  // approximately `anchorMillis * 1_000_000`, so later reads are that plus the
  // monotonic `System.nanoTime` delta. Initialized once per JMH state instance,
  // before warmup; `System.nanoTime` is monotonic, so the delta never goes
  // negative. Kept inline here (instead of delegating to `Time.wallClockNanos`)
  // so the benchmark keeps measuring this exact pipeline regardless of how the
  // `rdts.time.Time` implementation evolves.
  private val anchorMillis: Long = System.currentTimeMillis()
  private val anchorNanos: Long  = System.nanoTime()

  /** Raw `System.currentTimeMillis` – baseline to confirm the wrapper adds no overhead. */
  @Benchmark
  def systemCurrentTimeMillis(): Long =
    System.currentTimeMillis()

  /** Monotonic clock, not wall time (a comparison anchor for raw cost). */
  @Benchmark
  def nanoTime(): Long =
    System.nanoTime()

  /** `Instant.now` alone – isolates allocation cost from the conversion below. */
  @Benchmark
  def instantNow(): Instant =
    Instant.now()

  /** `Instant.now` plus conversion to a millisecond epoch long. */
  @Benchmark
  def instantToEpochMilli(): Long =
    Instant.now().toEpochMilli

  /** `Instant.now` converted to an epoch-nanos long (`getEpochSecond * 1e9 + getNano`).
    * On JDK 15+ (Linux) this genuinely has nanosecond resolution; on older
    * JDKs the `getNano` part is only a multiple of 1_000_000.
    */
  @Benchmark
  def instantToEpochNanos(): Long = {
    val instant = Instant.now()
    instant.getEpochSecond * 1_000_000_000L + instant.getNano
  }

  /** Wall-clock epoch-nanos with genuine nanosecond resolution, as a `Long` –
    * the wall-time counterpart of the monotonic `nanoTime` benchmark. The
    * anchoring logic lives in `rdts.time.Time`.
    */
  @Benchmark
  def wallClockNanos(): Time =
    Time.wallClockNanos()

  /** The same nanosecond-precision wall clock, delivered as a `java.time.Instant`
    * (inline anchored read, see `anchorMillis`). Functionally equivalent to
    * `Instant.now()` on JDK 15+; kept for cost comparison and as a portable
    * fallback for older JDKs.
    */
  @Benchmark
  def wallClockInstant(): Instant = {
    val totalNanos = anchorMillis * 1_000_000L + (System.nanoTime() - anchorNanos)
    Instant.ofEpochSecond(totalNanos / 1_000_000_000L, totalNanos % 1_000_000_000L)
  }
}
