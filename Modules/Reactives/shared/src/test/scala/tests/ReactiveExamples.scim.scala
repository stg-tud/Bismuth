/*:scim
= Reactive Programming Concepts
:label = manual
:flags = -hardwrap

The manual serves as an introduction of the concepts in reactive programming.
The full API is covered in the scaladoc especially for :m{Signals} and :m{Events}.
More details can be found in :cite{7, 3}.
The manual introduces the concepts related to functional reactive programming and event-based programming from a practical perspective.

Also see the introductory video lecture at :link{https://www.youtube.com/watch?v=iRh7UiclElk} that gives a step by step introduction.

- The chapter :ref{the-basics} covers how to get started and integrate reactives into a program, and
- The chapter :ref{common-combinators} presents the most common features for composing signals and events.
- The chapter :ref{combinators} describes other combinators.
- If you encounter any problems, check out the chapter :ref{common-pitfalls}.
- The readers interested in a more general presentation of these topics can find the essential references in the section :ref{related-work}.
*/

package tests.scim


import _root_.reactives.default.*

object ReactiveExamples {
  def main(): Unit = {

/*:scim
= Setup
:label = setup

Create a :m{build.sbt} file in an empty folder with the following contents:
*/

    /*
    // should also work on any recent version of the Scala 2.11 - 3 branches
    // including ScalaJS 1.0 and ScalaNative 0.4
    scalaVersion := "3.3.3"
    libraryDependencies += "de.tu-darmstadt.stg" %% "reactives" % "0.35.1"
    */

/*:scim
Or for Scala version 2.x:
*/

    /*
    // should also work on any recent version of the Scala 2.11 - 3 branches
    // including ScalaJS 1.0 and ScalaNative 0.4
    scalaVersion := "2.13.10"
    libraryDependencies += "de.tu-darmstadt.stg" %% "rescala" % "0.33.0"
    */

/*:scim
Install :link{http://www.scala-sbt.org/} sbt and run :m{sbt console} inside the folder,
this should allow you to follow along the following examples.

The code examples in the manual serve as a self-contained Scala REPL session.
Most code blocks can be executed on their own when adding this import,
but some require definitions from the prior blocks.
To use all features of reactives the only required import is:
*/

/*:scim
= The Basics
:label = the-basics

Reactives provide a way to work with time-changing values.
Such values normally originate from outside your program – examples are users that interact with a UI, network messages, or sensors of your device.

## Signals

Reactives are represented in Scala as additional data types that wrap or contain time-changing values.
We call reactives that always have a value :m{Signal[A]}.
A typical example could be a temperature sensor: The temperature changes over time, but there always is a current temperature.
*/

    val temperature: Signal[Double] = ???

/*:scim
The :m{???} operator is a Scala built-in that allows us to omit the definition for now, ideally we would use a library that provides signals, but we will see how to interact with typical imperative and callback based libraries shortly.

We can check if the temperature exceeds some limit:
*/

    val limit: Int = 10

    val exceedsLimit: Signal[Boolean] = Signal {
      temperature.value > limit
    }

/*:scim
Because the temperature changes over time, so does the result of an expression that uses the value of a signal.
We use the syntax :m{Signal { ... }} to provide a scope to such a :i{signal expression}.

A :m{Var[T]} holds a value of type :m{T}.
:m{Var[T]} is a subtype of :m{Signal[T]}. See also the chapter about Signals.
In contrast to declarative signals, :m{Var}s can be read and written to.
*/

    val va = Var(0)
    val vb = Var("Hello World")
    val vc = Var(List(1, 2, 3))
    val vd = Var((x: Int) => x * 2)

/*:scim
Vars enable the framework to track changes of input values.
Vars can be changed directly, via :m{set} and :m{transform}, which will trigger a propagation:
*/

    va.set(10)
    va.transform(value => value + 1)
    vc.transform(list => 0 :: list)

/*:scim
## Evt, fire

Imperative events are defined by the :m{Evt[T]} type.
:m{Evt[T]} are a subtype of :m{Event[T]}.
The value of the parameter :m{T} defines the value that is attached to the event.
If you do not care about the value, you can use an :m{Evt[Unit]}.
If you need more than one value to the same event, you can use tuples.
The following code snippet shows some valid event definitions:
*/

    val e1 = Evt[Int]()
    val e2 = Evt[Unit]()
    val e3 = Evt[(Boolean, String, Int)]()

/*:scim
Events can be fired with the method :m{fire}, which will start a propagation.
*/

    e1.fire(5)
    e2.fire(())
    e3.fire((false, "Hallo", 5))

/*:scim
## Now, observe, remove

The current value of a signal can be accessed using the :m{now} method.
It is useful for debugging and testing, and sometimes inside onclick handlers.
If possible, use observers or even better combinators instead.
*/

    assert(va.now == 11)
    assert(vb.now == "Hello World")
    assert(vc.now == List(0, 1, 2, 3))

/*:scim
The :m{observe} attaches a handler function to the event.
Every time the event is fired, the handler function is applied to the current value of the event.
*/

    val eObs = Evt[String]()
    val o1   = eObs.observe { x =>
      val string = "hello " + x + "!"
      println(string)
    }

    eObs.fire("annette")
    eObs.fire("tom")

/*:scim
If multiple handlers are registered, all of them are executed when the event is fired.
Applications should not rely on the order of handler execution.

Note that unit-type events still need an argument in the handler.
*/

    locally {
      val e = Evt[Unit]()
      e.observe(x => println("ping"))
      e.observe(_ => println("pong"))
    }

/*:scim
Note that events without arguments still need an argument in the handler.
*/

    {
      val eUnit = Evt[Unit]()
      eUnit.observe(x => println("ping"))
      eUnit.observe(_ => println("pong"))
    }

/*:scim
Scala allows one to refer to a method using the partially applied function syntax.
This approach can be used to directly register a method as an event handler.
*/

    def m1(x: Int) = {
      val y = x + 1
      println(y)
    }
    val e6 = Evt[Int]()
    val o4 = e6.observe(m1)

    e6.fire(10)

/*:scim
Handlers can be unregistered from events with the :m{remove} operator.
When a handler is unregistered, it is not executed when the event is fired.
If you create handlers, you should also think about removing them, when they are no longer needed.
*/

    val eRemove  = Evt[Int]()
    val handler1 = eRemove.observe(println)

    eRemove.fire(10)

    handler1.disconnect()

/*:scim
## Example

Now, we have introduced enough features to give a simple example.
The following example computes the displacement :m{space} of a particle that is moving at constant speed :m{SPEED}.
The application prints all the values associated to the displacement over time.
*/

    val SPEED  = 10
    val time   = Var(0)
    val space  = Signal { SPEED * time.value }
    val oSpace = space.observe((x: Int) => println(x))

    while time.now < 5 do {
      Thread.sleep(20)
      time.set(time.now + 1)
    }
    oSpace.disconnect()

/*:scim
The application behaves as follows.
Every 20 milliseconds, the value of the :m{time} var is increased by 1.
When the value of the :m{time} var changes,
the signal expression is reevaluated and the value of :m{space} is updated.
Finally, the current value of the :m{space} signal is printed every time the value of the signal changes.

Note that using :m{println(space.now)} would also print the value of the signal, but only at the point in time in which the print statement is executed.
Instead, the approach described so far prints :i{all} values of the signal.

= Common Combinators
:label = common-combinators

Combinators express functional dependencies among values.
Intuitively, the value of a combinator is computed from one or multiple input values.
Whenever any input changes, the value of the combinator is also updated.

## Latest, Changed

Conversion between signals and events are fundamental to introduce
time-changing values into OO applications -- which are usually event-based.

This section covers the basic conversions between signals and events.
The :m{latest} conversion function creates a signal from an event.
The signal holds the value associated to an event.
The value is held until the event is fired again and a new value is available.
The :m{changed} conversion function creates an event from a signal.
The function fires a new event every time a signal changes its value.

The :m{latest} function applies to an event and returns a signal
holding the latest value of the event :m{e}.
The initial value of the signal is set to :m{init}.

:code lang=scala
latest[T](e: Event[T], init: T): Signal[T]
*/

    // Example:
    val eLatest        = Evt[Int]()
    val s: Signal[Int] = eLatest.hold(10)
    assert(s.now == 10)

    eLatest.fire(1)
    assert(s.now == 1)

    eLatest.fire(2)
    assert(s.now == 2)

    eLatest.fire(1)
    assert(s.now == 1)

/*:scim
The :m{changed} function applies to a signal and returns an event
that is fired every time the signal changes its value.

:code lang=scala
changed[U >: T]: Event[U]
*/

    // Example:
    var testChanged          = 0
    val vChanged             = Var(1)
    val sChanged             = Signal { vChanged.value + 1 }
    val eChanged: Event[Int] = sChanged.changed
    val oChanged             = eChanged.observe((x: Int) => testChanged += 1)

    vChanged.set(2)
    assert(testChanged == 1)

    vChanged.set(3)
    assert(testChanged == 2)

/*:scim
## Map

The reactive :m{r.map f} is obtained by applying :m{f} to the value carried by :m{r}.
The map function must take the parameter as a formal parameter.
The return type of the map function is the type parameter value of the resulting event.
If :m{r} is a signal, then :m{r map f} is also a signal.
If :m{r} is an event, then :m{r map f} is also an event.
*/

    val sMap                  = Var[Int](0)
    val s_MAP: Signal[String] = sMap.map((x: Int) => x.toString)
    val oMap1                 = s_MAP.observe((x: String) => println(s"Here: $x"))

    val eMap                 = Evt[Int]()
    val e_MAP: Event[String] = eMap.map((x: Int) => x.toString)
    val oMap2                = e_MAP.observe((x: String) => println(s"Here: $x"))

    sMap.set(5)
    sMap.set(15)
    eMap.fire(2)
    eMap.fire(24)

/*:scim
## Fold

The :m{fold} function creates a signal by folding events with a
given function. Initially the signal holds the :m{init}
value. Every time a new event arrives, the function :m{f} is
applied to the previous value of the signal and to the value
associated to the event. The result is the new value of the signal.

:code lang=scala
fold[T,A](e: Event[T], init: A)(f: (A,T) => A): Signal[A]
*/

    // Example:
    val eFold              = Evt[Int]()
    val fFold              = (x: Int, y: Int) => x + y
    val sFold: Signal[Int] = eFold.fold(10)(fFold)

    eFold.fire(1)
    eFold.fire(2)
    assert(sFold.now == 13)

/*:scim
## Or, And

The event :m{e1 || e2} is fired upon the occurrence of one among :m{e1}
or :m{e2}. Note that the events that appear in the event expression
must have the same parameter type (:m{Int} in the next example).
The :m{or} combinator is left-biased, so if both :m{e1} and :m{e2} fire in the same
transaction, the left value is returned.
*/

    val eOr1     = Evt[Int]()
    val eOr2     = Evt[Int]()
    val e1_OR_e2 = eOr1 || eOr2
    val oOr      = e1_OR_e2.observe((x: Int) => println(x))

    eOr1.fire(1)
    eOr2.fire(2)

/*:scim
The event :m{e && p} (or the alternative syntax :m{e filter p}) is fired if :m{e} occurs and the predicate :m{p} is satisfied.
The predicate is a function that accepts the event parameter as a formal parameter and returns :m{Boolean}.
In other words the filter operator filters the events according to their parameter and a predicate.
*/

    val eAnd              = Evt[Int]()
    val e_AND: Event[Int] = eAnd.filter((x: Int) => x > 10)
    val oAnd              = e_AND.observe((x: Int) => println(x))

    eAnd.fire(5)
    eAnd.fire(3)
    eAnd.fire(15)
    eAnd.fire(1)
    eAnd.fire(2)
    eAnd.fire(11)

/*:scim
## Count Signal

Returns a signal that counts the occurrences of the event.
Initially, when the event has never been fired yet, the signal holds the value 0.
The argument of the event is simply discarded.

:code lang=scala
count(e: Event[_]): Signal[Int]
*/

    val eCount              = Evt[Int]()
    val sCount: Signal[Int] = eCount.count()

    assert(sCount.now == 0)
    eCount.fire(1); assert(sCount.now == 1)
    eCount.fire(3); assert(sCount.now == 2)

/*:scim
## Last(n) Signal

The :m{list} function returns a signal which holds the last :m{n} events.

Initially, an empty list is returned. Then the values are
progressively filled up to the size specified by the
programmer.
*/

    val eList                                          = Evt[Int]()
    val sList: Signal[scala.collection.LinearSeq[Int]] = eList.list(5)
    val oList                                          = sList.observe(println)

    eList.fire(1)
    eList.fire(2)
    eList.fire(3); eList.fire(4); eList.fire(5)
    eList.fire(6)

/*:scim
## List Signal

Collects the event values in a (growing) list. This function should be
used carefully. Since the entire history of events is maintained, the
function can potentially introduce a memory overflow.

:code lang=scala
list[T](e: Event[T]): Signal[List[T]]
*/

/*:scim
## LatestOption Signal

The :m{latestOption} function is a variant of the :m{latest}
function which uses the :m{Option} type to distinguish the case in
which the event did not fire yet. Holds the latest value of an event
as :m{Some(val)} or :m{None}.
*/

    val eOpt                      = Evt[Int]()
    val sOpt: Signal[Option[Int]] = eOpt.holdOption()
    assert(sOpt.now == None)
    eOpt.fire(1)
    assert(sOpt.now == Option(1))
    eOpt.fire(2)
    assert(sOpt.now == Option(2))
    eOpt.fire(1)
    assert(sOpt.now == Option(1))

/*:scim
## Fold matcher Signal

The :m{fold} :m{Match} construct allows to match on one of multiple events.
For every firing event, the corresponding handler function is executed,
to compute the new state.
If multiple events fire at the same time,
the handlers are executed in order.
The current parameter reflects the current state.
*/

    val word     = Evt[String]()
    val countFld = Evt[Int]()
    val reset    = Evt[Unit]()
    val resetB   = reset `branch` (_ => "")
    val wordB    = word `branch` identity
    val countB   = countFld `branch` (Fold.current * _)
    val result   = Fold("")(resetB, wordB, countB)
    val oResult  = result.observe(r => println(r))

    countFld.fire(10)
    reset.fire()
    word.fire("hello")
    countFld.fire(2)
    word.fire("world")
    transaction(countFld, word, reset) {
      countFld.admit(2)
      word.admit("do them all!")
      reset.admit(())
    }

/*:scim
## Iterate Signal

Returns a signal holding the value computed by :m{f} on the
occurrence of an event. Differently from :m{fold}, there is no
carried value, i.e. the value of the signal does not depend on the
current value but only on the accumulated value.

:code lang=scala
iterate[A](e: Event[_], init: A)(f: A => A): Signal[A]
*/

    // Example:
    var testIterValue: Int = 0
    val eIter              = Evt[Int]()
    val fIter              = (x: Int) => { testIterValue = x; x + 1 }
    val sIter: Signal[Int] = eIter.iterate(10)(fIter)

    eIter.fire(1)
    assert(testIterValue == 10)
    assert(sIter.now == 11)

    eIter.fire(2)
    assert(testIterValue == 11)
    assert(sIter.now == 12)

    eIter.fire(1)
    assert(testIterValue == 12)
    assert(sIter.now == 13)

/*:scim
## Change Event

The :m{change} function is similar to :m{changed}, but it
provides both the old and the new value of the signal in a tuple.

:code lang=scala
change[U >: T]: Event[(U, U)]
*/

    // Example:
    val sChange = Var(5)
    val eChange = sChange.change
    val oChange = eChange.observe(println)

    sChange.set(10)
    sChange.set(20)

/*:scim
## ChangedTo Event

The :m{changedTo} function is similar to :m{changed}, but it
fires an event only when the signal changes its value to a given
value.

:code lang=scala
changedTo[V](value: V): Event[Unit]
*/

    var testChangedTo           = 0
    val vChangedTo              = Var(1)
    val sChangedTo              = Signal { vChangedTo.value + 1 }
    val eChangedTo: Event[Unit] = sChangedTo.changed.filter(_ == 3).map(_ => ())
    val oChangedTo              = eChangedTo.observe((x: Unit) => testChangedTo += 1)

    assert(testChangedTo == 0)
    vChangedTo.set(2); assert(testChangedTo == 1)
    vChangedTo.set(3); assert(testChangedTo == 1)

/*:scim
## Flatten

The :m{flatten} function is used to "flatten" nested reactives.

It can, for instance, be used to detect if any signal within a collection of signals
fired a changed event:
*/

    val vFlatten1                   = Var(1)
    val vFlatten2                   = Var("Test")
    val vFlatten3                   = Var(true)
    val collection: List[Signal[?]] = List(vFlatten1, vFlatten2, vFlatten3)
    val innerChanges                = Signal { collection.map(_.changed).reduce((a, b) => a || b) }
    val anyChanged                  = innerChanges.flatten
    val oFlatten                    = anyChanged.observe(println)

    vFlatten1.set(10)
    vFlatten2.set("Changed")
    vFlatten3.set(false)

/*:scim
= Testing

Conventional testing methods fail to thoroughly test reactive applications.
Nodes may never be exposed to the full range of their possible inputs based on their current location in the spanned dependency graph.
Furthermore, on receiving an invalid input, it is impossible to trace back the route of the problem.
To tackle those shortcomings the :m{SimpleScheduler} adds the concept of :i{invariants} and :i{generators}.

## Invariants

Invariants can be directly attached to :m{Vars} and :m{Signals} to define functions that shall be true after every change.
Each node can have multiple invariants and they can be attached using :m{specify}.
*/

    val vInv = Var { 42 }

/*:scim
In the original REScala, invariants could be attached using :m{specify}.
This API is not available in this reactive library.

If an invariant fails an :m{InvariantViolationException} would be thrown.
The exception message would contain further information about the exception.
*/

/*:scim
= Common Pitfalls
:label = common-pitfalls

In this section we
collect the most common pitfalls for users that are new to reactive
programming.

## Accessing values in signal expressions

The :m{.value}
operator used on a signal or a var, inside a signal expression,
returns the signal/var value :i{and} creates a dependency. The
:m{now} operator returns the current value but does :i{not}
create a dependency. For example the following signal declaration
creates a dependency between :m{a} and :m{s}, and a dependency
between :m{b} and :m{s}.
*/

    val aPit = Var(42)
    val bPit = Var(42)
    val cPit = Signal { aPit.value + bPit.value }

/*:scim
The following code instead establishes only a dependency between
:m{b} and :m{s}.
*/

    val sPit = Signal { aPit.now + bPit.value }

/*:scim
In other words, in the last example, if :m{a} is updated, :m{s}
is not automatically updated. With the exception of the rare cases in
which this behavior is desirable, using :m{now} inside a signal
expression is almost certainly a mistake. As a rule of thumb, signals
and vars appear in signal expressions with the :m{.value} operator.

## Attempting to assign a signal

Signals are not assignable.
Signals depend on other signals and vars, the dependency is expressed by the signal expression.
The value of the signal is automatically updated when one of the values it depends on changes.
Any attempt to set the value of a signal manually is a mistake.

## Side effects in signal expressions

Signal expressions should be pure. i.e. they should not modify external variables.
For example the following code is conceptually wrong because the variable
:c is imperatively assigned from inside the signal expression.
*/

    /*
    /* WRONG - DON'T DO IT */
    val aSideWrong = Var(42)
    val bSideWrong = Var(42)

    var cSide = 0
    val sSideWrong = Signal{
      val sum: Int = aSideWrong.value + bSideWrong.value
      cSide = sum * 2           /* WRONG - DON'T DO IT */
    }
    assert(cSide == 4)
    */

/*:scim
A possible solution is to refactor the code above to a more functional
style. For example, by removing the variable :m{cSide} and replacing it
directly with the signal.
*/

    val aSide = Var(42)
    val bSide = Var(42)

    val cSide = Signal {
      val sum: Int = aSide.value + bSide.value
      sum * 2
    }
    assert(cSide.now == 4)

/*:scim
## Cyclic dependencies

When a signal :m{s} is defined, a dependency is established with each of the
signals or vars that appear in the signal expression of :m{s}.
Cyclic dependencies produce a runtime error and must be avoided.
For example the following code:
*/

    /*
    /* WRONG - DON'T DO IT */
    val aCyclic = Var(0)
    val sCyclic = Signal{ aCyclic.value + tCyclic.value }
    val tCyclic = Signal{ aCyclic.value + sCyclic.value + 1 }
    */

/*:scim
creates a mutual dependency between :m{s} and
:m{t}. Similarly, indirect cyclic dependencies must be avoided.

## Objects and mutability

Vars and signals may behave
unexpectedly with mutable objects. Consider the following example.
*/

    /*
    /* WRONG - DON'T DO THIS */
    class Foo(init: Int) { var x = init }
    val foo = new Foo(1)
    val varFoo = Var(foo)
    val s = Signal{ varFoo.value.x + 10 }
    println(s.now)
    foo.x = 2
    println(s.now)
    */

/*:scim
One may expect that after increasing the value of :m{foo.x},
the signal expression is evaluated again and updated
to 12. The reason why the application behaves differently is that
signals and vars hold :i{references} to objects, not the objects
themselves. When the value of the :m{x} field changes, the reference held by the
:m{varFoo} var is the same. For this reason, no change is detected
by the var, the var does not propagate the change to the signal, and
the signal is not reevaluated.

A solution to this problem is to use immutable objects. Since the
objects cannot be modified, the only way to change a field is to
create an entirely new object and assign it to the var. As a result,
the var is reevaluated.
*/

    class Foo(val x: Int) {}
    val fooImmut    = new Foo(1)
    val varFooImmut = Var(fooImmut)
    val sImmut      = Signal { varFooImmut.value.x + 10 }
    println(sImmut.now)
    varFooImmut.set(new Foo(2))
    println(sImmut.now)

/*:scim
Alternatively, one can still use mutable objects but assign again the
var to force the reevaluation. However this style of programming is
confusing for the reader and should be avoided when possible.
*/

    /*
    /* WRONG - DON'T DO THIS */
    class FooMut(init: Int) { var x = init }
    val fooMut = new FooMut(1)
    val varFooMut = Var(fooMut)
    val sMut = Signal{ varFooMut.value.x + 10 }
    println(sMut.now)
    fooMut.x = 2
    varFooMut set fooMut
    println(sMut.now)
    */

/*:scim
## Functions of reactive values

Functions that operate on
traditional values are not automatically transformed to operate on
signals. For example consider the following function:
*/

    def increment(x: Int): Int = x + 1

/*:scim
The following code does not compile because the compiler expects an
integer, not a var as a parameter of the :m{increment} function.
*/

    /*
    val aFunc = Var(1)
    val bFunc = increment(aFunc)     /* WRONG - DON'T DO THIS */
    val sFunc = Signal{ bFunc.value + 1 }
    */

/*:scim
The following code snippet is syntactically correct, but the signal
has a constant value 2 and is not updated when the var changes.
*/

    val aFunc      = Var(1)
    val bFunc: Int = increment(aFunc.now) // b is not reactive!
    val sFunc      = Signal { bFunc + 1 } // s is a constant signal with value 2

/*:scim
The following solution is syntactically correct and the signal
:m{sFunc} is updated every time the var :m{aFunc} is updated.
*/

    val aFunc2 = Var(1)
    val sFunc2 = Signal { increment(aFunc2.value) + 1 }

/*:scim
= Essential Related Work
:label = related-work

A more academic presentation of these concepts is in :cite{7}.
A complete bibliography on reactive programming is beyond the scope of this work.
The interested reader can refer to :cite{1} for an overview of reactive
programming and to :cite{8} for the issues concerning the integration of RP
with object-oriented programming.

This work builds on ideas originally developed in EScala :cite{3}
-- which supports event combination and implicit events.
Other reactive languages directly represent time-changing values and remove
inversion of control.
Among the others, we mention
FrTime :cite{2} (Scheme),
FlapJax :cite{6} (Javascript),
AmbientTalk/R :cite{4} and
Scala.React :cite{5} (Scala).

= Acknowledgments

Several people contributed to this manual,
among the others David Richter, Gerold Hintz and Pascal Weisenburger.

= References
:label = ref

[1] :i{A survey on reactive programming.}
E. Bainomugisha, A. Lombide Carreton, T. Van Cutsem, S. Mostinckx, and W. De Meuter.
ACM Comput. Surv. 2013.

[2] :i{Embedding dynamic dataflow in a call-by value language.}
G. H. Cooper and S. Krishnamurthi.
In ESOP, pages 294-308, 2006.

[3] :i{EScala: modular event-driven object interactions in Scala.}
V. Gasiunas, L. Satabin, M. Mezini, A. Nunez, and J. Noye.
AOSD '11, pages 227-240. ACM, 2011.

[4] :i{Loosely-coupled distributed reactive programming in mobile ad hoc networks.}
A. Lombide Carreton, S. Mostinckx, T. Cutsem, and W. Meuter.
In J. Vitek, editor, Objects, Models, Components, Patterns, volume 6141 of Lecture Notes in Computer Science, pages 41-60. Springer Berlin Heidelberg, 2010.

[5] :i{Deprecating the Observer Pattern with Scala.react.}
I. Maier and M. Odersky.
Technical report, 2012.

[6] :i{Flapjax: a programming language for ajax applications.}
L. A. Meyerovich, A. Guha, J. Baskin, G. H. Cooper, M. Greenberg, A. Bromfield, and S. Krishnamurthi.
OOPSLA '09, pages 1-20. ACM, 2009.

[7] :i{REScala: Bridging between object-oriented and functional style in reactive applications.}
G. Salvaneschi, G. Hintz, and M. Mezini.
AOSD '14, New York, NY, USA, Accepted for publication, 2014. ACM.

[8] :i{Reactive behavior in object-oriented applications: an analysis and a research roadmap.}
G. Salvaneschi and M. Mezini.
AOSD '13, pages 37-48, New York, NY, USA, 2013. ACM.
*/

  }
}
