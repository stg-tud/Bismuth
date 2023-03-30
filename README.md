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

See [Benchmarks.ipynb](./benchmarks/Benchmarks.ipynb) for benchmark results.


## Thoughts on the paper

We have collected some thoughts on the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf) that came to mind while implementing ITCs:

- The explanation, especially the graphical representation of events, were helpful to understand the intuition behind operations and their impact on the data structures.
- Conceptionally ITCs solve some issues regarding synchronization speed and conflict management of other approaches.
- Some parts of the operations are explained very short and may need a little more details. It took us some time to get the hang of how to implement it.
- Many operations require normalization for working correctly which can annihilate the performance advantage of ITCs. It should be more clear when to normalize and how to avoid it in some scenarios
- The encoder is explained really short and is missing the description of a decoder
- There is no description of what happens when ITCs get lost (i.e. failing nodes). Due to the nature of low syncing overhead there is no build-in way to know when other ITCs are forever lost. Obviously remaining ITCs get more fragmented, the addition of methods to clean ITCs can be helpful.


## License

The project is licensed under the [MIT License](./LICENSE).
