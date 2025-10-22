package tests.rescala.testtools

import reactives.SelectedScheduler.candidate.State
import reactives.core.CreationTicket

class ReevaluationBundle(val api: reactives.default.type) {
  import api.*

  class ReevaluationTracker[A, R[_]](reactive: R[A])(using turnSource: CreationTicket[State])
      extends munit.FunSuite {

    var results: List[A] = Nil
    /* should be private but is unused */
    // to prevent fake observers from being prematurely gc'd
    var strongRef: AnyRef = (reactive: @unchecked) match {
      case signal: Signal[?] => signal.map(reev)
      case event: Event[?]   => event.map(reev)
    }
    def reev(v1: Any): Any = {
      results ::= v1.asInstanceOf[A]
      v1
    }
    def assert(elements: A*)(using munit.Location): Unit = {
      assertEquals(results, elements.toList)
      ()
    }
    def assertClear(elements: A*)(using pos: munit.Location): Unit = {
      assert(elements*)
      results = Nil
    }
  }
}
