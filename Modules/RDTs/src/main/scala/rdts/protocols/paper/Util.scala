package rdts.protocols.paper

import rdts.base.Bottom

object Util:
  enum Agreement[+A]:
    case Invalid
    case Decided(value: A)
    case Undecided

  def updateIf[P: Bottom](condition: Boolean)(update: => P): P =
    if condition then
      update
    else
      Bottom[P].empty
