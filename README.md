# Interval Tree Clocks in Scala

This repository contains an implementation of Interval Tree Clocks in Scala based on the description of the [original paper](https://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf). Interval Tree Clocks (ITCs) are Data Structures for managing causality of events in distributed systems. As their implementation is based on branching, processing does not strictly depend on online communication but instead can work locally on a branch of the ITC. Additionally, the exact number of nodes is not necessarily needed as fork/join operations can be applied on the local branch without knowledge of cluster size.


## This implementation

We aimed at implementing all features of the original paper in a Scala library for reference in future applications. This implementation includes the basic data structure (id and event trees), the event counter operation, fork and join operations and normalize operation. For convenience, each (fork/join) operation is executed on a normalized ITC.

For reference, the repository includes implementations some similar methods, namely Dotted Version Vectors (DVV) and Vector Clocks (VC). These are used as comparison in the [benchmarks](#benchmark).

We included the encoder proposed in the paper and added a decoder to efficiently share ITCs between peers while using an encoding that uses as little bits as possible. The encoder exposes methods to write to byte array and string and works on top of Scalas BigInt implementation to store interim encodings memory efficient and allow for build-in bit shifting.
Decoding a bit sequence stops as soon as a correct ITC has been read; leading zeros will be ignored.
De-/encoders for VCs and DVVs are included as well and share the same API.

Unit tests for relevant code are included. The tests use property-based testing to efficiently check correct behavior on arbitrary inputs using generators.


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
