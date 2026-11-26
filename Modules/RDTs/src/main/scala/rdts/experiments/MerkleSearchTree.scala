package rdts.experiments

import pt.kcry.blake3.Blake3
import rdts.base.{Bottom, Lattice}
import rdts.experiments.MerkleSearchTree.{Empty, Encoder, Hash, Node, Tree}

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint}

/** An ordered key value store encoded as a Merkle Search Tree (Auvolat & Taïani, SRDS 2019).
  *
  * The shape of the tree depends only on the set of stored keys, never on the order of operations.
  * Each key is assigned a level by hashing it with BLAKE3 and counting the leading zero digits in base [[MerkleSearchTree.base]].
  * A node at level `l` stores consecutive keys of level `l`, and its children hold the keys in between (all with a lower level).
  * Each node is identified by the BLAKE3 hash of its keys, values, and child hashes,
  * thus two trees (or subtrees) with the same contents have the same hash.
  *
  * The [[Ordering]] of keys must be consistent with the [[Encoder]]: keys that compare equal must have the same encoding.
  *
  * If `V` is a lattice, then the tree is a lattice as well, with pointwise merging of values.
  * Use [[update]] (not [[put]] or [[remove]]) to keep modifications monotonic.
  */
final class MerkleSearchTree[K, V] private (val root: Tree[K, V])(using
    ordering: Ordering[K],
    keyEncoder: Encoder[K],
    valueEncoder: Encoder[V]
) {

  def rootHash: Hash = root.hash

  def isEmpty: Boolean = root == Empty

  /** Number of entries, O(1). */
  def size: Int = root.size

  def get(key: K): Option[V] = MerkleSearchTree.lookup(root, key)

  def contains(key: K): Boolean = get(key).isDefined

  /** Inserts or replaces the value of `key`. */
  def put(key: K, value: V): MerkleSearchTree[K, V] = upsert(key, _ => value)

  /** Inserts `value`, or merges it into the existing value of `key`. */
  def update(key: K, value: V)(using Lattice[V]): MerkleSearchTree[K, V] =
    upsert(key, existing => existing.fold(value)(Lattice.merge(_, value)))

  /** Sets the value of `key` to `f(currentValue)`. */
  def upsert(key: K, f: Option[V] => V): MerkleSearchTree[K, V] =
    wrap(upsertTree(root, key, levelOf(key), f))

  def remove(key: K): MerkleSearchTree[K, V] = {
    val newRoot = removeTree(root, key, levelOf(key))
    if newRoot eq root then this else wrap(newRoot)
  }

  /** All entries in ascending key order. */
  def iterator: Iterator[(K, V)] = entries(root)

  /** Entries with `from <= key < until` in ascending key order. */
  def range(from: K, until: K): Iterator[(K, V)] = rangeEntries(root, from, until)

  def keys: Iterator[K] = iterator.map(_._1)

  def toMap: Map[K, V] = iterator.toMap

  /** Pointwise merge, skipping subtrees with equal hashes. */
  def merge(other: MerkleSearchTree[K, V])(using Lattice[V]): MerkleSearchTree[K, V] = {
    val newRoot = union(root, other.root)
    if newRoot eq root then this else wrap(newRoot)
  }

  override def equals(obj: Any): Boolean = obj match
      case other: MerkleSearchTree[?, ?] => rootHash == other.rootHash
      case _                             => false

  override def hashCode(): Int = rootHash.hashCode()

  override def toString: String = iterator.mkString(s"MerkleSearchTree($rootHash)(", ", ", ")")

  // ---------------------------------------------------------------------------------------------------------------- //
  // Implementation. All functions below produce the canonical tree for the resulting set of entries.

  private def wrap(tree: Tree[K, V]): MerkleSearchTree[K, V] = new MerkleSearchTree(tree)

  private def levelOf(key: K): Int = MerkleSearchTree.levelOf(keyEncoder.encode(key))

  /** Smart constructor: a node without keys collapses into its only child. */
  private def node(level: Int, keys: Vector[K], values: Vector[V], children: Vector[Tree[K, V]]): Tree[K, V] =
    if keys.isEmpty then children.head
    else Node.create(level, keys, values, children)

  /** Splits `tree` into the entries with keys smaller and larger than `key`, and the value of `key` if present. */
  private def split(tree: Tree[K, V], key: K): (Tree[K, V], Option[V], Tree[K, V]) = tree match
      case Empty         => (Empty, None, Empty)
      case n: Node[K, V] =>
        n.keys.search(key) match
            case Found(i) =>
              (
                node(n.level, n.keys.take(i), n.values.take(i), n.children.take(i + 1)),
                Some(n.values(i)),
                node(n.level, n.keys.drop(i + 1), n.values.drop(i + 1), n.children.drop(i + 1))
              )
            case InsertionPoint(i) =>
              val (left, value, right) = split(n.children(i), key)
              (
                node(n.level, n.keys.take(i), n.values.take(i), n.children.take(i) :+ left),
                value,
                node(n.level, n.keys.drop(i), n.values.drop(i), right +: n.children.drop(i + 1))
              )

  /** Concatenates two trees, where all keys in `left` are smaller than all keys in `right`. */
  private def join(left: Tree[K, V], right: Tree[K, V]): Tree[K, V] = (left, right) match
      case (Empty, _)                     => right
      case (_, Empty)                     => left
      case (l: Node[K, V], r: Node[K, V]) =>
        if l.level > r.level then
            node(l.level, l.keys, l.values, l.children.updated(l.children.size - 1, join(l.children.last, r)))
        else if l.level < r.level then
            node(r.level, r.keys, r.values, r.children.updated(0, join(l, r.children.head)))
        else
            val middle = join(l.children.last, r.children.head)
            node(l.level, l.keys ++ r.keys, l.values ++ r.values, (l.children.init :+ middle) ++ r.children.tail)

  private def upsertTree(tree: Tree[K, V], key: K, keyLevel: Int, f: Option[V] => V): Tree[K, V] = tree match
      case Empty =>
        node(keyLevel, Vector(key), Vector(f(None)), Vector(Empty, Empty))
      case n: Node[K, V] if n.level < keyLevel =>
        // key becomes the new root of this subtree, which can not contain key because of its lower level
        val (left, _, right) = split(n, key)
        node(keyLevel, Vector(key), Vector(f(None)), Vector(left, right))
      case n: Node[K, V] =>
        n.keys.search(key) match
            case Found(i) =>
              node(n.level, n.keys, n.values.updated(i, f(Some(n.values(i)))), n.children)
            case InsertionPoint(i) if n.level == keyLevel =>
              val (left, _, right) = split(n.children(i), key)
              node(
                n.level,
                n.keys.patch(i, List(key), 0),
                n.values.patch(i, List(f(None)), 0),
                n.children.patch(i, List(left, right), 1)
              )
            case InsertionPoint(i) =>
              node(n.level, n.keys, n.values, n.children.updated(i, upsertTree(n.children(i), key, keyLevel, f)))

  /** Returns `tree` itself (reference equal) if `key` is not contained. */
  private def removeTree(tree: Tree[K, V], key: K, keyLevel: Int): Tree[K, V] = tree match
      case Empty                               => tree
      case n: Node[K, V] if n.level < keyLevel => tree
      case n: Node[K, V]                       =>
        n.keys.search(key) match
            case Found(i) =>
              node(
                n.level,
                n.keys.patch(i, Nil, 1),
                n.values.patch(i, Nil, 1),
                n.children.patch(i, List(join(n.children(i), n.children(i + 1))), 2)
              )
            case InsertionPoint(_) if n.level == keyLevel => tree
            case InsertionPoint(i)                        =>
              val child = removeTree(n.children(i), key, keyLevel)
              if child eq n.children(i) then tree
              else node(n.level, n.keys, n.values, n.children.updated(i, child))

  /** Pointwise merge of `left` and `right`, where `left` is passed as the first argument to the value lattice. */
  private def union(left: Tree[K, V], right: Tree[K, V])(using Lattice[V]): Tree[K, V] = (left, right) match
      case (Empty, _)                               => right
      case (_, Empty)                               => left
      case (l: Node[K, V], r: Node[K, V]) if l == r => l
      case (l: Node[K, V], r: Node[K, V])           =>
        // the keys of the result at the highest level are the union of the top-level keys of both sides,
        // cutting both trees at these keys yields pairs of subtrees with lower levels that are merged recursively
        val level = math.max(l.level, r.level)
        val cuts  = mergeSorted(
          if l.level == level then l.keys else Vector.empty,
          if r.level == level then r.keys else Vector.empty
        )
        val (lPieces, lValues) = splitMany(l, cuts)
        val (rPieces, rValues) = splitMany(r, cuts)
        val values             = cuts.indices.map { i =>
          (lValues(i), rValues(i)) match
              case (Some(lv), Some(rv)) => Lattice.merge(lv, rv)
              case (Some(lv), None)     => lv
              case (None, Some(rv))     => rv
              case (None, None)         => throw IllegalStateException("cut key missing from both trees")
        }.toVector
        val children = lPieces.indices.map(i => union(lPieces(i), rPieces(i))).toVector
        val result   = node(level, cuts, values, children)
        if result == l then l else result

  /** Merges two sorted key vectors, removing duplicates. */
  private def mergeSorted(left: Vector[K], right: Vector[K]): Vector[K] = {
    val res = Vector.newBuilder[K]
    var i   = 0
    var j   = 0
    while i < left.size || j < right.size do
        if j >= right.size then { res += left(i); i += 1 }
        else if i >= left.size then { res += right(j); j += 1 }
        else
            val c = ordering.compare(left(i), right(j))
            if c < 0 then { res += left(i); i += 1 }
            else if c > 0 then { res += right(j); j += 1 }
            else { res += left(i); i += 1; j += 1 }
    res.result()
  }

  /** Cuts `tree` at the sorted `cuts` into `cuts.size + 1` trees containing the keys between consecutive cuts.
    * Also returns the values of the cut keys, if contained in `tree`.
    * Each resulting node is constructed (and hashed) only once.
    */
  private def splitMany(tree: Tree[K, V], cuts: Vector[K]): (Vector[Tree[K, V]], Vector[Option[V]]) =
    if cuts.isEmpty then (Vector(tree), Vector.empty)
    else
        tree match
            case Empty         => (Vector.fill(cuts.size + 1)(Empty), Vector.fill(cuts.size)(None))
            case n: Node[K, V] =>
              val pieces      = Vector.newBuilder[Tree[K, V]]
              val found       = Vector.newBuilder[Option[V]]
              val segKeys     = scala.collection.mutable.ArrayBuffer.empty[K]
              val segValues   = scala.collection.mutable.ArrayBuffer.empty[V]
              val segChildren = scala.collection.mutable.ArrayBuffer.empty[Tree[K, V]]

              // the current segment always ends with a child when cut
              def cut(value: Option[V]): Unit = {
                pieces += node(n.level, segKeys.toVector, segValues.toVector, segChildren.toVector)
                found += value
                segKeys.clear()
                segValues.clear()
                segChildren.clear()
              }

              var c = 0 // next cut
              n.children.indices.foreach { i =>
                val isLast = i == n.keys.size
                val start  = c
                while c < cuts.size && (isLast || ordering.lt(cuts(c), n.keys(i))) do c += 1
                val (childPieces, childFound) = splitMany(n.children(i), cuts.slice(start, c))
                segChildren += childPieces.head
                childPieces.iterator.drop(1).zip(childFound).foreach { (piece, value) =>
                  cut(value)
                  segChildren += piece
                }
                if !isLast then
                    if c < cuts.size && ordering.equiv(cuts(c), n.keys(i)) then
                        cut(Some(n.values(i)))
                        c += 1
                    else
                        segKeys += n.keys(i)
                        segValues += n.values(i)
              }
              pieces += node(n.level, segKeys.toVector, segValues.toVector, segChildren.toVector)
              (pieces.result(), found.result())

  private def entries(tree: Tree[K, V]): Iterator[(K, V)] = tree match
      case Empty         => Iterator.empty
      case n: Node[K, V] =>
        n.children.indices.iterator.flatMap { i =>
          entries(n.children(i)) ++ (if i < n.keys.size then Iterator.single((n.keys(i), n.values(i)))
                                     else Iterator.empty)
        }

  private def rangeEntries(tree: Tree[K, V], from: K, until: K): Iterator[(K, V)] = tree match
      case Empty         => Iterator.empty
      case n: Node[K, V] =>
        // keys in [start, end) are within range; children in (start, end) are entirely within range
        val start = n.keys.search(from).insertionPoint
        val end   = n.keys.search(until).insertionPoint
        (start to end).iterator.flatMap { i =>
          val child =
            if i == start || i == end then rangeEntries(n.children(i), from, until)
            else entries(n.children(i))
          child ++ (if i < end then Iterator.single((n.keys(i), n.values(i))) else Iterator.empty)
        }
}

object MerkleSearchTree {

  /** Keys are assigned levels by writing their hash in base `2^bitsPerDigit`. */
  inline val bitsPerDigit = 4

  /** The base B of the paper, i.e., the expected number of children of each inner node. */
  inline val base = 1 << bitsPerDigit

  inline val hashLength = 32

  def empty[K: {Ordering, Encoder}, V: Encoder]: MerkleSearchTree[K, V] = new MerkleSearchTree(Empty)

  def apply[K: {Ordering, Encoder}, V: Encoder](entries: (K, V)*): MerkleSearchTree[K, V] =
    entries.foldLeft(empty[K, V]) { case (tree, (k, v)) => tree.put(k, v) }

  given bottom[K: {Ordering, Encoder}, V: Encoder]: Bottom[MerkleSearchTree[K, V]] = Bottom.provide(empty)

  given lattice[K, V: Lattice]: Lattice[MerkleSearchTree[K, V]] = (left, right) => left.merge(right)

  /** Number of leading zero digits (in base [[base]]) of the BLAKE3 hash of the encoded key. */
  def levelOf(encodedKey: Array[Byte]): Int = {
    val hash     = Blake3.hash(encodedKey, hashLength)
    var zeroBits = 0
    var i        = 0
    var done     = false
    while !done && i < hash.length do
        val b = hash(i) & 0xff
        if b == 0 then zeroBits += 8
        else
            zeroBits += Integer.numberOfLeadingZeros(b) - 24
            done = true
        i += 1
    zeroBits / bitsPerDigit
  }

  @tailrec
  private def lookup[K, V](tree: Tree[K, V], key: K)(using Ordering[K]): Option[V] = tree match
      case Empty         => None
      case n: Node[K, V] =>
        n.keys.search(key) match
            case Found(i)          => Some(n.values(i))
            case InsertionPoint(i) => lookup(n.children(i), key)

  /** Deterministic binary encoding of keys and values, used for hashing. */
  @FunctionalInterface
  trait Encoder[A] {
    def encode(value: A): Array[Byte]
  }

  object Encoder {
    given bytes: Encoder[Array[Byte]] = identity
    given string: Encoder[String]     = _.getBytes(StandardCharsets.UTF_8)
    given int: Encoder[Int]           = v => Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)
    given long: Encoder[Long]         = v => Array.tabulate(8)(i => (v >>> (56 - 8 * i)).toByte)
    given boolean: Encoder[Boolean]   = v => Array(if v then 1.toByte else 0.toByte)
    given unit: Encoder[Unit]         = _ => Array.emptyByteArray
  }

  /** A 256 bit BLAKE3 hash. */
  final class Hash private[MerkleSearchTree] (private[MerkleSearchTree] val bytes: Array[Byte]) {
    def toArray: Array[Byte] = bytes.clone()

    def toHex: String = bytes.map(b => f"${b & 0xff}%02x").mkString

    override def toString: String = toHex.take(16)

    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = obj match
        case other: Hash => java.util.Arrays.equals(bytes, other.bytes)
        case _           => false
  }

  object Hash {
    def fromArray(bytes: Array[Byte]): Hash =
        require(bytes.length == hashLength)
        new Hash(bytes.clone())
  }

  /** A (sub)tree in canonical form. Nodes are immutable and identified by their hash. */
  sealed trait Tree[+K, +V] {
    def hash: Hash
    def size: Int
  }

  case object Empty extends Tree[Nothing, Nothing] {
    val hash: Hash = new Hash(Array.ofDim(hashLength))
    def size: Int  = 0
  }

  /** `children(i)` contains the keys between `keys(i - 1)` and `keys(i)`, thus `children.size == keys.size + 1`.
    * All keys have level `level`, and all children have a lower level.
    */
  final class Node[+K, +V] private (
      val level: Int,
      val keys: Vector[K],
      val values: Vector[V],
      val children: Vector[Tree[K, V]],
      val hash: Hash,
      val size: Int,
  ) extends Tree[K, V] {
    override def equals(obj: Any): Boolean = obj match
        case other: Node[?, ?] => hash == other.hash
        case _                 => false

    override def hashCode(): Int = hash.hashCode()

    override def toString: String = s"Node($level, ${keys.mkString("[", ", ", "]")}, $hash)"
  }

  object Node {
    private[MerkleSearchTree] def create[K, V](
        level: Int,
        keys: Vector[K],
        values: Vector[V],
        children: Vector[Tree[K, V]]
    )(using keyEncoder: Encoder[K], valueEncoder: Encoder[V]): Node[K, V] = {
      assert(keys.nonEmpty && keys.size == values.size && children.size == keys.size + 1)
      val hasher = Blake3.newHasher()
      hasher.update(level)
      hasher.update(keys.size)
      keys.indices.foreach { i =>
        hasher.update(children(i).hash.bytes)
        val k = keyEncoder.encode(keys(i))
        hasher.update(k.length)
        hasher.update(k)
        val v = valueEncoder.encode(values(i))
        hasher.update(v.length)
        hasher.update(v)
      }
      hasher.update(children.last.hash.bytes)
      val size = keys.size + children.iterator.map(_.size).sum
      new Node(level, keys, values, children, new Hash(hasher.done(hashLength)), size)
    }
  }
}
