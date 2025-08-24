package dag

import scala.collection.immutable.{HashMap, Map, Set, List}
import java.security.KeyPair
import crypto.Ed25519Util

// a hash directed acyclic graph
case class HashDAG[T] private(
                               // the state S of a HashDAG consists of the vertices and edges contained in the HashDAG, S = (V, E)
                               // but separately saving the vertices and edges is redundant,
                               // a better way to solve this is by mapping each vertex to the set of its children
                               graph: Map[Event[T], Set[Event[T]]],
                               authorKeys: KeyPair,
                               queue: Set[Event[T]]
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
  private def getCurrentHeads: Set[Event[T]] =
    Set.from(graph.filter((_, set) => set.isEmpty).map((event, _) => event))

  def generator(content: T): Event[T] = {
    val currentHeads = getCurrentHeads
    val currentHeadsIDs = currentHeads.map(event => event.id)
    val signature = Ed25519Util.sign(content.toString.getBytes, authorKeys.getPrivate)
    
    Event(Some(content), authorKeys.getPublic, currentHeadsIDs, signature)
  }

  def effector(event: Event[T]): HashDAG[T] =
    assume(!contains(event), "Event already in HashDAG!")

    if event.calculateHash != event.id || !event.signatureIsValid then
      return this

    val dependencies = event.dependencies
    if dependencies.forall(e => contains(e)) then
      var g = this.graph
      for (d <- dependencies)
        val e = g.find((e, _) => e.id == d).get._1
        g = g.updated(e, g(e) + event)
        
      HashDAG(g + (event -> Set.empty), this.authorKeys, this.queue - event)
    else
      HashDAG(this.graph, this.authorKeys, this.queue + event)


  def addEvent(content: T): HashDAG[T] =
    // generate the event
    val currentHeads = getCurrentHeads
    val event = generator(content)

    // apply the event
    effector(event)
    
  def processQueue(): HashDAG[T] =
    var hashDAG = this
    for (event <- this.queue) 
      hashDAG = effector(event)
      
    hashDAG
    
  def getEventByID(id: String): Event[T] =
    graph.find((e, _) => e.id == id).get._1
    
  def pathExists(from: Event[T], to: Event[T], visited: Set[Event[T]] = Set()): Boolean = 
    def dfs(current: Event[T], visited: Set[Event[T]]): Boolean = 
      if (current == to) true
      else if (visited.contains(current)) false
      else 
        val neighbours = graph.getOrElse(current, Nil)
        neighbours.exists(neighbor => dfs(neighbor, visited + current))

    dfs(from, Set())


object HashDAG:
  def apply[T](authorKeys: KeyPair): HashDAG[T] =
    val graph = new HashMap[Event[T], Set[Event[T]]]()
    val root = new Event("0", None, authorKeys.getPublic, Set.empty, Array.empty).asInstanceOf[Event[T]]

    new HashDAG[T](graph.updated(root, Set.empty), authorKeys, Set.empty)
