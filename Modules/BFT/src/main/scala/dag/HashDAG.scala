package dag

import scala.collection.immutable.{HashMap, Map, Set}
import scala.collection.mutable

// a hash directed acyclic graph
case class HashDAG[T] private(
                               // the state S of a HashDAG consists of the vertices and edges contained in the HashDAG, S = (V, E)
                               // but separately saving the vertices and edges is redundant,
                               // a better way to solve this is by mapping each vertex to the set of its children
                               graph: Map[Event[T], Set[Event[T]]],
                               authorKeys: KeyPair,
                             ):

  // checks if an event is contained in the graph
  def contains(event: Event[T]): Boolean = graph.contains(event)

  def contains(id: String): Boolean =
    graph.exists((e, _) => e.id == id)

  // checks for the existence of child vertices
  def hasChild(event: Event[T]): Boolean =
    assume(contains(event), "The given node is not part of the HashDAG!")

    graph(event).nonEmpty

  // returns the current forward extremities (aka Heads)
  def getCurrentHeads: Set[Event[T]] =
    Set.from(graph.filter((_, set) => set.isEmpty).map((event, _) => event))

  def generator(content: T): Event[T] = {
    val currentHeads = getCurrentHeads
    val currentHeadsIDs = currentHeads.map(event => event.id)
    Event(Some(content), authorKeys.privateKey, currentHeadsIDs)
  }

  def effector(event: Event[T]): HashDAG[T] =
    assume(!contains(event), "Event already in HashDAG!")

    val i = event.calculateHash
    if event.calculateHash != event.id then
      return this

    val dependencies = event.dependencies
    if dependencies.forall(e => contains(e)) then
      var g = this.graph
      for (d <- dependencies)
        val e = g.find((e, _) => e.id == d).get._1
        g = g.updated(e, g(e) + event)

      HashDAG(g + (event -> Set.empty), this.authorKeys)
    else
      // TODO: add the event to waiting list and process it as soon as all its dependencies are received
      this


  def addEvent(content: T): HashDAG[T] =
    // generate the event
    val currentHeads = getCurrentHeads
    val event = generator(content)

    // apply the event
    effector(event)


object HashDAG:
  def apply[T](authorKeys: KeyPair): HashDAG[T] =
    val graph = new HashMap[Event[T], Set[Event[T]]]()
    val root = new Event("0", None, authorKeys.publicKey, Set.empty, "").asInstanceOf[Event[T]]

    new HashDAG[T](graph.updated(root, Set.empty), authorKeys)




case class KeyPair(
                    publicKey: Array[Byte],
                    privateKey: Array[Byte],
                  )

