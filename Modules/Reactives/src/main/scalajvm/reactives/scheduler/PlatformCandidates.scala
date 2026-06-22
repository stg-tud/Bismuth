package reactives.scheduler

import reactives.core.{DynamicScope, Scheduler}

object PlatformCandidates {

  transparent inline def furtherSelect(inline selection: String): Option[parrp.type] =
    inline selection match
        case "parrp" => Some(parrp)
        case other   => None

  object parrp extends GlobalCandidate[reactives.parrp.ParRPDefault.ParRPState] {
    override def scheduler: Scheduler[State]       = reactives.parrp.ParRPDefault.scheduler
    override def dynamicScope: DynamicScope[State] = reactives.parrp.ParRPDefault.scheduler.dynamicScope
  }

}
