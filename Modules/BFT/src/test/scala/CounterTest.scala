import datatypes.Counter

class CounterTest extends munit.FunSuite:

  test("create an event") {
    val counterA1 = Counter()
    val (counterA2, event1) = counterA1.add(1)

    assert(counterA2.value == 1, "event creation failed")

  }

  test("create more than one event") {
    val counterA1 = Counter()
    val (counterA2, event1) = counterA1.add(1)
    val (counterA3, event2) = counterA2.add(2)

    assert(counterA3.value == 3, "event creation failed")

  }

  test("sync two counters") {
    val counterA1 = Counter()
    val (counterA2, event1) = counterA1.add(1)

    val counterB1 = Counter()
    val (counterB2, event2) = counterB1.add(2)

    val counterA3 = counterA2.receiveEvent(event2)
    val counterB3 = counterB2.receiveEvent(event1)

    assert(counterA3.value == counterB3.value, true)

  }
