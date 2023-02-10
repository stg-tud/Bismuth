# Interval Tree Clocks in Scala

This repository contains an implementation of Interval Tree Clocks in Scala based on the description of the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf). Interval Tree Clocks (ITCs) are Data Structures for managing causality of events in distributed systems. As their implementation is based on branching, processing does not strictly depend on online communication but instead can work locally on a branch of the ITC. Additionally, the exact number of nodes is not necessarily needed as fork/join operations can be applied on the local branch without knowledge of cluster size.


## This implementation

We aimed at implementing all features of the original paper in a Scala library for reference in future applications. This implementation includes the basic data structure (id and event trees), the event counter operation, fork and join operations and normalize operation. We also included the de-/encoder proposed in the paper to efficiently share ITCs between peers.

For reference, the repository includes implementations some similar methods, namely Dotted Version Vectors and Vector Clocks, and their respective de-/encoders.

Unit tests for relevant code are included.


## Benchmark


## License

The project is licensed under the [MIT License](./LICENSE).
