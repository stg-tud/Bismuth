package rdts.protocols

import rdts.base.Bottom

object Util:
    enum Agreement[+A]:
        case Invalid
        case Decided(value: A)
        case Undecided

    def precondition[P: Bottom](condition: Boolean)(update: => P): P =
      if condition then
          update
      else
          Bottom[P].empty
