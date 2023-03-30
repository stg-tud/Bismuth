# Interval Tree Clocks in Scala

This repository contains an implementation of Interval Tree Clocks in Scala based on the description of the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf) as well as an evaluation of our implementation and the behaviour of Interval Tree Clocks in general.

## Short description of Interval Tree Clock
Interval Tree Clocks (ITCs) are Data Structures for managing causality of events in distributed systems and can be used in place of Vector Clocks and Version Vectors.
They are specifically designed to be used in highly dynamic systems, i.e., when group membership of replicas changes frequently.
This is an area where Vector Clocks and Version Vectors struggle, as there is no native way for them to shrink, i.e., removing replicas from the timestamps, leading to a monotonically growing size of the timestamps.
In dynamic systems, this either means that you need to rely on complicated, error-prone and possibly expensive mechanisms to reduce overhead, or to simply to accept the increasing overhead of causality management when using Version Vectors or Vector Clocks.
Interval Tree Clocks on the other hand can grow and shrink with replicas entering and leaving the system.
This is due to the way how IDs are assigned to replicas and how the IDs are used in the process of advancing the event tracking mechanism.
Contrary to the scalar IDs and event counters of the vector based approaches, Interval Tree Clocks use tree representations for both the IDs (IdTrees) and the event tracking (EventTrees).

More details on how Interval Tree Clocks work can be found in the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf) as well as in [this blog post](https://ferd.ca/interval-tree-clocks.html).


## Notes on this implementation

This repository contains tested implementations of all features of the original paper as a Scala library for reuse in applications as well as for further evaluation.
This implementation includes the data structures of Interval Tree Clocks (`IdTree` and `EventTree`), as well as the operations on them.
All of the user-facing operations that produce modified trees only produce normalized trees but don't assume normalized trees as input.
We provide two interfaces for using Interval Tree Clocks in place of Vector Clocks and Version Vectors in the Fork-Event-Join-Model: `ForkEventJoinClock` and `ForkEventJoinVersioning`.

For reference, the repository includes implementations for other causality management solutions, most notably a `Map`-based Vector Clock that is used in the [benchmark](#benchmark).

We implemented an encoder and a decoder for the binary encoding proposed in the paper.
The proposed encoding tries to minimize the number of bits used to encode the trees and therefore focuses on size rather than encoding speed.
There are two implementations for the ITC encoders and decoders in this repository. One is based on BigDecimals and does not heavily rely on lower level bitwise operations, while the other one uses byte arrays and relies heavily on bitwise operations. The second implementation was needed because of the huge overhead found in the first implementation. The byte array based implementation can be found in `FastIntervalTreeClockEncoder` and is orders of magnitude faster than the `IntervalTreeClockEncoder`.

Encoders and decoders are also implemented for the other causality management approaches found in this repository. They share the same interface and are composable.

Unit tests can be found in `src/test`. This includes manually written test cases as well as tests based on property-based testing using generated test inputs.


## Benchmark

See [Benchmarks.ipynb](./benchmarks/Benchmarks.ipynb) for the results and a description of the benchmarks.

TL;DR: Our ITC implementation performed better than our Map-based VC implementation in all benchmark scenarios, both in the static as well as in dynamic scenarios.


## Thoughts on the paper

We have collected some thoughts on the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf) that came to mind while implementing ITCs:

- We think that overall it is both an excellent paper and a really elegant approach to causality management.
- The explanation, especially the graphical representation of events, were helpful to understand the intuition behind operations and their impact on the data structures.
- ITCs seem to effectively solve the scalability issues in causality management that are found in other approaches.
- The paper gives system designers a great framework for causality management, since their great scalability and explicit system model make them quite flexible.
- While the paper is relatively light on evaluation, their results seem to be accurate. Our evaluation also backs their claims. ITCs seem to be quite scalable and provide great performance both in dynamic and static systems.
- While the paper does a good job at explaining how ITCs can be implemented, some of the operations are only covered briefly. Compared to Vector Clocks, which are quite simple to implement, ITCs are much more complex and the operations quite subtle in some places.
- Some operations in the paper assume normalized inputs to work correctly, this should be made more explicit in the paper.
- The impact of normalization on performance is not covered in the paper.
- The definition of an encoding is not explained and is missing the description of a decoder. While coming up with a working decoder is not that hard, it feels like this should be documented (maybe in an extended version of the paper).
- The authors don't talk about what happens when replicas fail. When replicas don't resurface, their part of the ID space can't be used in normalization. Frequent failures can affect the performance, since the trees can't be simplified in places where there are holes in the ID space. We came up with a few approaches on how this could be dealt with, so solutions for specific applications exist, but they are outside of the ITC logic.
- ITCs have comparatively complex operation semantics due to their use of trees instead of scalar values.
- While they introduce a system model based on forking and joining for creating and "retiring" of replicas, they don't map this to more concrete models and give example for potential use real-world applications

## Open research / evaluation questions
- How do ITCs perform in real-world systems? Which scenarios are problematic?
- How does the choice which replica to fork / join affect performance? (worst-case vs. best-case and different strategies)
- How does fragmentation of the ID space through silent failures of replicas affect performance in the long run?
- What strategies exist to combat this fragmentation? How safe are they?
- How to decide which replica to fork / join with different assumptions? (knowledge of all replica IDs vs. knowledge of random selection vs. structured)
- How does normalization affect performance?
- Can servers lend IDs to clients instead of using Dotted Version Vectors to solve sibling explosion?
- Is it possible to modify ITCs to separate the event identifier from the causal history? Just like in Dotted Version Vectors: essentially Dotted Interval Tree Clocks.

## License

The project is licensed under the [MIT License](./LICENSE).
