# Bismuth

Research software of the [Software Technology Group](https://www.stg.tu-darmstadt.de/) at TU Darmstadt on programming foundations for distributed software.

See the [website](https://stg-tud.github.io/Bismuth/) for an overview of the research, the [libraries](https://stg-tud.github.io/Bismuth/documentation/) with their manuals, and [how to build and work with this repository](https://stg-tud.github.io/Bismuth/documentation/#development).


# Contributing

By contributing code to this repository, you agree to publish it under the Apache 2.0 License and confirm that you have the necessary rights to do so.

Bismuth is a monorepo. The core libraries are under active research and change often, and many experiments and student projects build on top of them. Keeping everything in one repository means core changes are applied and tested against all experiments, and experiments immediately benefit from core improvements. This works best if everyone stays close to `main`:

* Push small, incremental changes directly to `main`, and pull often. Applying core changes later during a merge is much more painful.
* The newest commit on `main` should always compile and pass the tests. It is nice if intermediate commits do as well, but less important.
* Rebase your own commits on top of `main` before pushing (`git config set pull.rebase true`). Merge commits are fine to group a series of changes or when resolving real conflicts. Merge your branch into `main` (so `main` is the first parent), not `main` into your branch.
* Avoid long-lived feature branches. They tend to get lost and never merged. Use a branch to propose a change for review, but do not let it diverge indefinetly.
* Do not force push to `main` if someone may depend on the commit. Push a fixup commit instead.
