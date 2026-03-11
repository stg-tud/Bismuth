package lofi_acl.travelplanner

import munit.FunSuite
import rdts.base.{LocalUid, Uid}
import rdts.filters.{Filter, PermissionTree}

class TravelPlanTest extends FunSuite {
  given uid: LocalUid              = LocalUid(Uid.gen())
  given filter: Filter[TravelPlan] = summon[Filter[TravelPlan]]

  test("changeTitle") {
    var uut = TravelPlan.empty
    uut = uut.merge(uut.setTitle("Test"))
    assertEquals(uut.title.read, "Test")
    uut = uut.merge(uut.setTitle("Test 2"))
    assertEquals(uut.title.read, "Test 2")

    val delta1 = uut.setTitle("A")
    val delta2 = uut.setTitle("B")

    assertEquals(uut.merge(delta1).merge(delta2).title.read, "B")
    assertEquals(uut.merge(delta2).merge(delta1).title.read, "B")
  }

  test("Filter expenses") {
    val expense = TravelPlan.empty.addExpense("Test", "123.12€")
    assert(filter.isAllowed(
      expense,
      PermissionTree.fromPathSet(Set(
        "expenses.inner.*.value.description",
        "expenses.inner.*.value.amount",
        "expenses.inner.*.dots",
      ))
    ))
  }
}
