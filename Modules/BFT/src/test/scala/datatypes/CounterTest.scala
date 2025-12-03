package datatypes

import datatypes.Counter

import scala.concurrent.duration.*

class CounterTest extends munit.FunSuite:

    private val testSetSize = if isCI then 40 else 150

    override def munitTimeout: Duration = 5.minutes

    test("create an event") {
      var counter = Counter()
      counter = counter.merge(counter.add(1))

      assertEquals(counter.value, 1)

    }

    test("create more than one event") {
      var counter = Counter()
      counter = counter.merge(counter.add(1))
      counter = counter.merge(counter.add(2))

      assertEquals(counter.value, 3)

    }

    test("sync two counters: example 1") {
      var counter1 = Counter()
      var counter2 = Counter()

      val delta1 = counter1.add(1)
      counter1 = counter1.merge(delta1)

      val delta2 = counter2.add(2)
      counter2 = counter2.merge(delta2)

      counter1 = counter1.merge(delta2)
      counter2 = counter2.merge(delta1)

      assertEquals(counter1.value, 3)
      assertEquals(counter1.value, counter2.value)

    }

    test("sync two counters: example 2") {
      var counter1 = Counter()
      var counter2 = Counter()
      var counter3 = Counter()
      var counter4 = Counter()

      var deltas = List[Counter]()
      for i <- 0 to testSetSize do
          var delta = Counter()
          if i % 2 == 0 then {
            delta = counter2.add(i)
            counter2 = counter2.merge(delta)
          } else if i % 3 == 0 then
              delta = counter3.add(i)
              counter3 = counter3.merge(delta)
          else
              delta = counter1.add(i)
              counter1 = counter1.merge(delta)

          deltas = deltas :+ delta

      for delta <- deltas do
          counter1 = counter1.merge(delta)

      for window <- deltas.sliding(10, 5) do
          var deltaGroup = Counter()
          for d <- window do
              deltaGroup = deltaGroup.merge(d)

          counter2 = counter2.merge(deltaGroup)

      deltas = deltas.reverse

      for delta <- deltas do
          counter3 = counter3.merge(delta)

      for window <- deltas.sliding(20, 10) do
          var deltaGroup = Counter()
          for d <- window do
              deltaGroup = deltaGroup.merge(d)

          counter4 = counter4.merge(deltaGroup)

      assertEquals(counter1.value, counter2.value)
      assertEquals(counter3.value, counter4.value)
      assertEquals(counter1.value, counter3.value)

    }
