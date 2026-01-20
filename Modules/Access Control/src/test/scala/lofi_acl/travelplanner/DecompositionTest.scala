package lofi_acl.travelplanner

import munit.FunSuite
import rdts.base.{Lattice, LocalUid, Uid}

class DecompositionTest extends FunSuite {

  given uid: LocalUid = LocalUid(Uid("id42"))

  private val mutators: Vector[TravelPlan => TravelPlan] = Vector(
    _.setTitle("Test"),
    _.addBucketListEntry("Test"),
    tp => tp.setBucketListEntryText(tp.bucketList.entries.head._1, "Hello World"),
    tp => tp.removeBucketListEntry(tp.bucketList.entries.head._1),
    _.addExpense("Test", "42.11 €"),
    tp => tp.setExpenseAmount(tp.expenses.entries.head._1, "11.11 €"),
    tp => tp.setExpenseDescription(tp.expenses.entries.head._1, "Test"),
    tp => tp.setExpenseComment(tp.expenses.entries.head._1, "ABC"),
    tp => tp.removeExpense(tp.expenses.entries.head._1),
  )

  test("produces correct amount of decomposed deltas") {
    var instance = TravelPlan.empty
    mutators.zipWithIndex.foreach((mutator, idx) =>
        val delta = mutator(instance)
        instance = delta.merge(instance)
        if idx != 4 then assertEquals(delta.decomposed.size, 1)
        else delta.decomposed.size == 2
    )
  }

  test("equivalence") {
    var instance = TravelPlan.empty
    mutators.foreach(mutator =>
        val delta = mutator(instance)
        instance = delta.merge(instance)
        assertEquals(delta.decomposed.reduceLeft((l, r) => Lattice.merge(l, r)), delta)
        assertEquals(delta.decomposed.reduceRight((l, r) => Lattice.merge(l, r)), delta)
    )
  }
}
