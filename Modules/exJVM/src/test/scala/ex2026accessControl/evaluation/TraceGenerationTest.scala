package ex2026accessControl.evaluation

import munit.FunSuite
import replication.authz.Authorization

import scala.util.Random

class TraceGenerationTest extends FunSuite {

  test("generateEventGraph produces a graph that materializes without throwing, for varying concurrency") {
    for concurrencyProbability <- Seq(0.0, 0.3, 1.0) do
        given Random = Random(42)
        val generated = TraceGeneration.generateEventGraph(
          numReplicas = 5,
          numEvents = 100,
          minEntriesPerMapPerReplica = 2,
          maxEntriesPerMapPerReplica = 10,
          concurrencyProbability = concurrencyProbability
        )

        // genesis + one delegation per non-root replica + all decomposed delta events
        assert(generated.eventGraph.events.size > 100)

        // Should not throw and should be deterministic given the same graph/store.
        val state = Authorization.materialize(generated.eventGraph, generated.deltaValueStore)
        assertEquals(Authorization.materialize(generated.eventGraph, generated.deltaValueStore), state)
  }

  test("concurrencyProbability = 0 yields a graph with a single head; = 1 yields multiple heads") {
    val sequential = TraceGeneration.generateEventGraph(
      numReplicas = 4,
      numEvents = 40,
      minEntriesPerMapPerReplica = 2,
      maxEntriesPerMapPerReplica = 10,
      concurrencyProbability = 0.0
    )(using Random(1))
    assertEquals(sequential.eventGraph.heads.size, 1)

    val concurrent = TraceGeneration.generateEventGraph(
      numReplicas = 4,
      numEvents = 40,
      minEntriesPerMapPerReplica = 2,
      maxEntriesPerMapPerReplica = 10,
      concurrencyProbability = 1.0
    )(using Random(1))
    assert(concurrent.eventGraph.heads.size > 1, "expected more than one head")
  }
}
