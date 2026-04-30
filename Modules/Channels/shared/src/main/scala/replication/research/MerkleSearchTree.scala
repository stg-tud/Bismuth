package replication.research

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import pt.kcry.blake3.Blake3

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

/** A simple persistent Merkle search tree for timestamped, already-encoded deltas.
  *
  * Entries are ordered by `(timestamp, entryHash)`, so timestamps need not be globally unique.
  * The entry hash is computed over serialized timestamp bytes and serialized payload bytes.
  *
  * The tree stores encoded payload bytes directly and every node can be materialized as a self-contained block,
  * making it straightforward to persist blocks to disk or exchange them over the network.
  *
  * Origin of the idea: “Merkle Search Trees: Efficient State-Based CRDTs in Open Networks”
  * by Alex Auvolat and François Taïani.
  * File still vibecoded, but less offensively so. <- said the clanker that coded this file.
  */
final case class MerkleSearchTree[K] private (
    rootNode: Option[MerkleSearchTree.Node[K]],
    entriesByHash: Map[MerkleSearchTree.Hash, MerkleSearchTree.Entry[K]],
    branchingFactor: Int,
    ordering: Ordering[K],
) {
  import MerkleSearchTree.*

  private val keyOrdering: Ordering[EntryKey[K]] = new Ordering[EntryKey[K]] {
    override def compare(x: EntryKey[K], y: EntryKey[K]): Int = {
      val timestampCmp = ordering.compare(x.timestamp, y.timestamp)
      if timestampCmp != 0 then timestampCmp else x.hash.hex.compareTo(y.hash.hex)
    }
  }
  private def normalizedBranchingFactor: Int = math.max(2, branchingFactor)

  def isEmpty: Boolean = entriesByHash.isEmpty
  def size: Int        = entriesByHash.size

  def rootHash: Option[Hash] = rootNode.map(_.hash)

  def entries: Vector[Entry[K]] = rootNode.fold(Vector.empty)(_.allEntries)

  def contains(hash: Hash): Boolean = entriesByHash.contains(hash)

  def blocks: Vector[Block] = rootNode.toVector.flatMap(_.allBlocks)

  def rootBlock: Option[Block] = rootNode.map(_.block)

  def insert[V: JsonValueCodec](timestamp: K, value: V)(using JsonValueCodec[K]): MerkleSearchTree[K] =
    insertEncoded(Entry.fromValue(timestamp, value))

  def insertEncoded(entry: Entry[K]): MerkleSearchTree[K] =
    if contains(entry.hash) then this
    else {
      val newRoot = rootNode match {
        case None => Some(Node.Leaf(Vector(entry)))
        case Some(root) =>
          val grown = Node.insert(root, entry, normalizedBranchingFactor, keyOrdering)
          Some(if grown.size == 1 then grown.head else Node.Branch(grown))
      }

      copy(rootNode = newRoot, entriesByHash = entriesByHash.updated(entry.hash, entry), branchingFactor = normalizedBranchingFactor)
    }

  def remove(hash: Hash): MerkleSearchTree[K] =
    entriesByHash.get(hash) match {
      case None => this
      case Some(entry) =>
        val newRoot = rootNode.flatMap(Node.remove(_, entry.key, normalizedBranchingFactor, keyOrdering)).map(Node.normalizeRoot)
        copy(rootNode = newRoot, entriesByHash = entriesByHash.removed(hash), branchingFactor = normalizedBranchingFactor)
    }

  def update[V: JsonValueCodec](hash: Hash, timestamp: K, value: V)(using JsonValueCodec[K]): MerkleSearchTree[K] =
    updateEncoded(hash, Entry.fromValue(timestamp, value))

  def updateEncoded(oldHash: Hash, replacement: Entry[K]): MerkleSearchTree[K] =
    remove(oldHash).insertEncoded(replacement)

  /** Returns the entries present in this tree but missing in `other`. */
  def missingFrom(other: MerkleSearchTree[K]): Vector[Entry[K]] =
    (rootNode, other.rootNode) match {
      case (None, _)                   => Vector.empty
      case (Some(local), None)         => local.allEntries
      case (Some(local), Some(remote)) => compareNodes(local, remote)
    }

  private def compareNodes(local: Node[K], remote: Node[K]): Vector[Entry[K]] =
    if local.hash == remote.hash then Vector.empty
    else
        (local, remote) match {
          case (Node.Leaf(localEntries), Node.Leaf(remoteEntries)) =>
            diffEntries(localEntries, remoteEntries)

          case (Node.Branch(localChildren), Node.Branch(remoteChildren)) =>
            localChildren.iterator.flatMap { child =>
              val overlaps = remoteChildren.filter(otherChild => Node.intersects(child, otherChild, keyOrdering))
              overlaps.size match {
                case 0 => child.allEntries
                case 1 => compareNodes(child, overlaps.head)
                case _ => diffEntries(child.allEntries, overlaps.iterator.flatMap(_.allEntries).toVector)
              }
            }.toVector

          case _ =>
            diffEntries(local.allEntries, remote.allEntries)
        }

  private def diffEntries(localEntries: Vector[Entry[K]], remoteEntries: Vector[Entry[K]]): Vector[Entry[K]] = {
    val remoteHashes = remoteEntries.iterator.map(_.hash).toSet
    localEntries.filterNot(entry => remoteHashes.contains(entry.hash))
  }
}

object MerkleSearchTree {

  final case class Hash(hex: String) extends AnyVal

  object Hash {
    def blake3(parts: Iterable[Array[Byte]]): Hash = {
      val hasher = Blake3.newHasher()
      parts.foreach { part =>
        hasher.update(intBytes(part.length))
        hasher.update(part)
      }
      Hash(hasher.doneHex(64))
    }

    private def intBytes(value: Int): Array[Byte] =
      ByteBuffer.allocate(4).putInt(value).array()
  }

  final case class Block(hash: Hash, bytes: Array[Byte])

  final case class Entry[K](timestamp: K, encoded: Array[Byte], hash: Hash) {
    lazy val key: EntryKey[K] = EntryKey(timestamp, hash)
  }

  object Entry {
    def fromValue[K: JsonValueCodec, V: JsonValueCodec](timestamp: K, value: V): Entry[K] = {
      val encoded = writeToArray(value)
      Entry(timestamp, encoded, entryHash(timestamp, encoded))
    }
  }

  final case class EntryKey[K](timestamp: K, hash: Hash)

  sealed trait Node[K] {
    def hash: Hash
    def block: Block
    def minKey: EntryKey[K]
    def maxKey: EntryKey[K]
    def allEntries: Vector[Entry[K]]
    def allBlocks: Vector[Block]
  }

  object Node {
    final case class Leaf[K](entries: Vector[Entry[K]]) extends Node[K] {
      require(entries.nonEmpty)

      override lazy val minKey: EntryKey[K]        = entries.head.key
      override lazy val maxKey: EntryKey[K]        = entries.last.key
      override lazy val allEntries: Vector[Entry[K]] = entries
      override lazy val block: Block               = {
        val bytes = encodeLeafBlock(entries)
        Block(Hash.blake3(List(bytes)), bytes)
      }
      override lazy val hash: Hash                 = block.hash
      override def allBlocks: Vector[Block]       = Vector(block)
    }

    final case class Branch[K](children: Vector[Node[K]]) extends Node[K] {
      require(children.nonEmpty)

      override lazy val minKey: EntryKey[K]          = children.head.minKey
      override lazy val maxKey: EntryKey[K]          = children.last.maxKey
      override lazy val allEntries: Vector[Entry[K]] = children.flatMap(_.allEntries)
      override lazy val block: Block                 = {
        val bytes = encodeBranchBlock(children)
        Block(Hash.blake3(List(bytes)), bytes)
      }
      override lazy val hash: Hash                   = block.hash
      override lazy val allBlocks: Vector[Block]     = block +: children.flatMap(_.allBlocks)
    }

    def normalizeRoot[K](node: Node[K]): Node[K] = node match {
      case Branch(children) if children.size == 1 => normalizeRoot(children.head)
      case other                                  => other
    }

    def intersects[K](left: Node[K], right: Node[K], keyOrdering: Ordering[EntryKey[K]]): Boolean =
      keyOrdering.lteq(left.minKey, right.maxKey) && keyOrdering.lteq(right.minKey, left.maxKey)

    def contains[K](node: Node[K], key: EntryKey[K], keyOrdering: Ordering[EntryKey[K]]): Boolean =
      keyOrdering.lteq(node.minKey, key) && keyOrdering.lteq(key, node.maxKey)

    def insert[K](node: Node[K], entry: Entry[K], branchingFactor: Int, keyOrdering: Ordering[EntryKey[K]]): Vector[Node[K]] =
      node match {
        case Leaf(entries) =>
          val updated = insertEntry(entries, entry, keyOrdering)
          if updated.size <= branchingFactor then Vector(Leaf(updated))
          else splitEntries(updated).map(Leaf(_))

        case Branch(children) =>
          val childIndex      = findChildIndexForInsert(children, entry.key, keyOrdering)
          val grownChild      = insert(children(childIndex), entry, branchingFactor, keyOrdering)
          val updatedChildren = children.patch(childIndex, grownChild, 1)
          rebalanceChildren(updatedChildren, branchingFactor)
      }

    def remove[K](node: Node[K], key: EntryKey[K], branchingFactor: Int, keyOrdering: Ordering[EntryKey[K]]): Option[Node[K]] =
      node match {
        case Leaf(entries) =>
          val updated = entries.filterNot(_.key == key)
          Option.when(updated.nonEmpty)(Leaf(updated))

        case Branch(children) =>
          val childIndex = children.indexWhere(child => contains(child, key, keyOrdering))
          if childIndex < 0 then Some(node)
          else {
            val updatedChildren = remove(children(childIndex), key, branchingFactor, keyOrdering) match {
              case Some(child) => children.updated(childIndex, child)
              case None        => children.patch(childIndex, Nil, 1)
            }

            updatedChildren.size match {
              case 0 => None
              case 1 => Some(updatedChildren.head)
              case _ => Some(Branch(updatedChildren))
            }
          }
      }

    private def insertEntry[K](entries: Vector[Entry[K]], entry: Entry[K], keyOrdering: Ordering[EntryKey[K]]): Vector[Entry[K]] = {
      val idx = entries.indexWhere(existing => keyOrdering.gt(existing.key, entry.key))
      if idx < 0 then entries :+ entry else entries.patch(idx, Seq(entry), 0)
    }

    private def splitEntries[K](entries: Vector[Entry[K]]): Vector[Vector[Entry[K]]] = {
      val midpoint = entries.size / 2
      Vector(entries.take(midpoint), entries.drop(midpoint)).filter(_.nonEmpty)
    }

    private def rebalanceChildren[K](children: Vector[Node[K]], branchingFactor: Int): Vector[Node[K]] =
      if children.size <= branchingFactor then Vector(Branch(children))
      else children.grouped(branchingFactor).map(group => Branch(group.toVector)).toVector

    private def findChildIndexForInsert[K](
        children: Vector[Node[K]],
        key: EntryKey[K],
        keyOrdering: Ordering[EntryKey[K]],
    ): Int = {
      val exact = children.indexWhere(child => contains(child, key, keyOrdering))
      if exact >= 0 then exact
      else children.indexWhere(child => keyOrdering.lt(key, child.minKey)) match {
        case -1    => children.size - 1
        case found => math.max(0, found)
      }
    }

    private def encodeLeafBlock[K](entries: Vector[Entry[K]]): Array[Byte] =
      encodeParts(entries.flatMap(entry => Vector(entry.hash.hex.getBytes(StandardCharsets.UTF_8), entry.encoded)))

    private def encodeBranchBlock[K](children: Vector[Node[K]]): Array[Byte] =
      encodeParts(children.map(_.hash.hex.getBytes(StandardCharsets.UTF_8)))

    private def encodeParts(parts: Seq[Array[Byte]]): Array[Byte] = {
      val totalSize = parts.iterator.map(_.length + 4).sum
      val buffer    = ByteBuffer.allocate(totalSize)
      parts.foreach { part =>
        buffer.putInt(part.length)
        buffer.put(part)
      }
      buffer.array()
    }
  }

  def empty[K](branchingFactor: Int = 16)(using ordering: Ordering[K]): MerkleSearchTree[K] =
    MerkleSearchTree(None, Map.empty, math.max(2, branchingFactor), ordering)

  def fromEntries[K: Ordering: JsonValueCodec, V: JsonValueCodec](
      rawEntries: Iterable[(K, V)],
      branchingFactor: Int = 16,
  ): MerkleSearchTree[K] =
    fromEncodedEntries(rawEntries.iterator.map { case (timestamp, value) => Entry.fromValue(timestamp, value) }.toVector, branchingFactor)

  def fromEncodedEntries[K](
      rawEntries: Iterable[Entry[K]],
      branchingFactor: Int = 16,
  )(using ordering: Ordering[K]): MerkleSearchTree[K] = {
    val keyOrdering = new Ordering[EntryKey[K]] {
      override def compare(x: EntryKey[K], y: EntryKey[K]): Int = {
        val timestampCmp = ordering.compare(x.timestamp, y.timestamp)
        if timestampCmp != 0 then timestampCmp else x.hash.hex.compareTo(y.hash.hex)
      }
    }
    val entryOrdering = new Ordering[Entry[K]] {
      override def compare(x: Entry[K], y: Entry[K]): Int = keyOrdering.compare(x.key, y.key)
    }
    val entries       = rawEntries.iterator.toVector.distinctBy(_.hash).sorted(using entryOrdering)
    val tree          = empty[K](branchingFactor)
    entries.foldLeft(tree)((acc, entry) => acc.insertEncoded(entry))
  }

  def entryHash[K: JsonValueCodec](timestamp: K, encoded: Array[Byte]): Hash =
    Hash.blake3(List(writeToArray(timestamp), encoded))

  def entryHash[K: JsonValueCodec, V: JsonValueCodec](timestamp: K, delta: V): Hash =
    entryHash(timestamp, writeToArray(delta))
}
