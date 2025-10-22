package test.rdts.bespoke

import rdts.base.Lattice
import rdts.syntax.DeltaBuffer

class WorkflowExample extends munit.FunSuite {

  enum Workflow {
    case Init()
    case Documents(hasStaffSheet: Boolean = false, hasHourConfirmation: Boolean = false)
    case Contract(signed: Boolean = false)
    case Complete()

    def newContract: Workflow         = Documents()
    def addStaffSheet: Workflow       = Documents(hasStaffSheet = true)
    def addHourConfirmation: Workflow = Documents(hasHourConfirmation = true)
    def createContract: Workflow      = this match
      case Documents(hasStaffSheet = true, hasHourConfirmation = true) => Contract()
      case other                                                       => Init()

    def signContract: Workflow = this match
      case Contract(signed = false) => Contract(true)
      case other                    => Init()

    def complete: Workflow = this match
      case Contract(signed = true) => Complete()
      case other                   => Init()
  }

  given Lattice[Workflow] = {
    given Lattice[Workflow.Init]      = Lattice.derived
    given Lattice[Boolean]            = Lattice.fromOrdering
    given Lattice[Workflow.Documents] = Lattice.derived
    given Lattice[Workflow.Contract]  = Lattice.derived
    given Lattice[Workflow.Complete]  = Lattice.derived
    Lattice.sumLattice
  }

  test("workflow") {
    val db     = DeltaBuffer(Workflow.Init())
    val result = db
      .mod(_.newContract)
      .mod(_.addStaffSheet)
      .mod(_.addHourConfirmation)
      .mod(_.createContract)
      .mod(_.signContract)
      .mod(_.complete)

    assertEquals(result.state, Workflow.Complete())

  }
}
