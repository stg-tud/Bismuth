package com.github.ckuessner.causality

trait NormalForm[T]:
    extension (tree: T) def normalized: T
