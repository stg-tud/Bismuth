package datatypes

import dag.HashDAG
import java.security.PublicKey

trait Replica[T, R <: Replica[T, R]]:
  def id: Array[Byte]
  def hashDAG: HashDAG[T]
  def merge(other: R): R
  def generateDelta(ids: List[String]): R
