Well firstly let me explain what this projects tried to implement.

Using the PRDTS I tried to implement a Chimeric Protocol with 
Paxos as it's voting mechanism and the FBAS quorum as the selection/decision criterion.
Now lets talk about the structure of the project itself

-Bismuth
--Modules
---RDTs
----src
-----protocols
------chimeric


The first implemented and the most basic and simple ones to implement and help me get into the project were

-- Chimeric.scala, ChimericTest.scala and FBAS.scala

This is the implementation of using existing paxos voting  PRDTs and adding the FBAS quorum instead of majority voting in a closed network setup i.e. no new nodes can add into network or no exisitng node can remove itself from it either.


After finishing this simple version, I moved onto the open network setup where nodes can change their trust slices based on the fact that nodes can add or be removed at any time.

But first I had to create a an open network setup where nodes can be added and removed without violating conditions that hold the network true. 

OpenNetwork.scala is the file where all of this enabled. It controls state transition and checks the validity of the states as well. 

FBASOpen.scala is the Quorum check with an additional feature of allowing a state transition only when quorumIntersection is true.

ChimericOpen.scala now also uses the OpenNetwork to implement the Chimeric Protocol in Open Network setup.

And finally 

ChimericOpenTest.scala tests the ChimericOpen with a addition and removal of nodes using the chimeric protocol.

For Future work, 
Test the protocol across multiple systems, where each maintains its own local state and exchanges updates with others.

Another extension could represent configuration state locally, allowing systems to observe or propose different paths concurrently. This would lead to having multiple final states where sometimes the merges between these states may or may not ahppen depending upon the transgression over these paths. To Solve this, one can work on teh QuorumIntersection function in the FBASOpen file.


Running the Project

The complete information for running the Bismuth repo can be found at Bismuth/README.md

Here is the instructions for installing and running the code.

Firstly install [coursier](https://get-coursier.io/docs/cli-installation) – a single binary called `cs` – and then run `cs launch sbt` in the project root directory. This provides you with the sbt shell, where you can type `compile` or `test` to ensure that everything is working correctly. If you get strange errors you may be using a too new/old java version, try `cs launch --jvm=21 sbt` to force the use of Java 21 (will be downloaded).

For IDE setup , I used Metals (a language server): https://scalameta.org/metals/

You can also use IntelliJ with the Scala plugin: https://www.jetbrains.com/help/idea/get-started-with-scala.html

Then type project rdts into the sbt shell to test the files in this project.

You can then run these test files using 

"testOnly test.rdts.protocols.ChimericOpenTest"

or 

"testOnly test.rdts.protocols.ChimericTest"

