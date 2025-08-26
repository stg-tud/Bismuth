package ex2024travel.lofi_acl.sync

import ex2024travel.lofi_acl.travelplanner.model.TravelPlanModel

trait TravelPlanModelFactory {
  def createAsRootOfTrust: TravelPlanModel

  def createByJoining(invite: String): TravelPlanModel

  def createAsRootOfTrustWithExampleData: TravelPlanModel = {
    val model = createAsRootOfTrust

    model.changeTitle("Portugal Trip")
    model.addBucketListEntry("Porto")
    model.addBucketListEntry("Lisbon")
    model.addBucketListEntry("Faro")
    model.addExpense("Ice Cream", "3.14 €")

    model
  }
}
