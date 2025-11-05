package loreCompilerPlugin

import lore.dsl.*
import loreCompilerPlugin.annotation.LoReProgram
import reactives.default.{Signal as Derived, Var as Source}

@LoReProgram
object test:
    val counter: Source[Int]      = Source(1)
    val doubled: Derived[Int]     = Derived { counter.value * 2 }
    val incremented: Derived[Int] = Derived { doubled.value + 1 }

    val add_one = Interaction[Int, Int]
      .executes { (curr: Int, n: Int) => curr + 1 }

    val inc_counter = add_one.modifies { counter }

    val inc_max_10 = inc_counter
      .requires { (curr: Int, _: Int) => curr < 10 }
      .ensures { (curr: Int, n: Int) => curr <= 10 }

    val counter_max_10: Invariant = Invariant(counter.value <= 10)
end test
