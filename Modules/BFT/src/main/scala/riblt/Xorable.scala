package riblt

trait Xorable[A]:
  extension (a1: A) def xor(a2: A): A
  extension (a: A) def identity: A
