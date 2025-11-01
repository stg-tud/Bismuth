package dag

import scala.collection.immutable.{HashMap, List, Map, Set}
import java.security.{PublicKey, PrivateKey}
import crypto.Ed25519Util

// a hash directed acyclic graph
case class HashDAG[T](
                        graph: Map[String, Set[String]],
                        events: Map[String, Event[T]],
                        publicKey: PublicKey,
                        privateKey: Option[PrivateKey],
                        queue: Set[Event[T]] = Set.empty[Event[T]],
                        byzantineNodes: Set[PublicKey] = Set.empty
                      ):

  // checks if an event is contained in the graph
  def contains(event: Event[T]): Boolean =
    contains(event.id)

  def contains(id: String): Boolean =
    graph.contains(id)

  def contains(node: PublicKey): Boolean =
    events.values.exists(e => e.author == node)

  // checks for the existence of child vertices
  def hasChild(event: Event[T]): Boolean =
    hasChild(event.id)

  def hasChild(id: String): Boolean =
    assume(contains(id), "The given node is not part of the HashDAG!")

    graph(id).nonEmpty

  // returns the current forward extremities (aka Heads)
  def getCurrentHeads: Set[Event[T]] =
    getCurrentHeadsIDs.map(id => events(id))

  def getCurrentHeadsIDs: Set[String] =
    graph.filter((_, set) => set.isEmpty).keySet

  def generator(content: T): Event[T] = {
    val signature = Ed25519Util.sign(content.toString.getBytes, privateKey.get)

    Event(Some(content), publicKey, getCurrentHeadsIDs, signature)
  }

  def merge(delta: HashDAG[T]): HashDAG[T] =
    var result = this

    for event <- delta.events.values do
      result = result.effector(event)

    for event <- delta.queue do
      result = result.effector(event)

    var i = result.queue.size
    while i != 0 do
      i = 0
      for event <- result.queue do
        result = result.effector(event)
        if result.contains(event) then i += 1

    result

  def effector(event: Event[T]): HashDAG[T] =
    if contains(event) then
      this
    else
      if event.calculateHash != event.id || !event.signatureIsValid then
        this
      else if isByzantine(event) then
        // markEventsFromByzantineNode(event.author)

        this.copy(byzantineNodes = this.byzantineNodes + event.author)
      else
        val dependencies = event.dependencies
        if dependencies.forall(e => contains(e)) then
          var g = this.graph
          for d <- dependencies do
            val e = events(d)
            g = g.updated(d, g(d) + event.id)

          HashDAG(
            g + (event.id           -> Set.empty),
            this.events + (event.id -> event),
            this.publicKey,
            this.privateKey,
            this.queue - event,
            this.byzantineNodes,
          )
        else
          HashDAG(this.graph, this.events, this.publicKey, this.privateKey, this.queue + event, this.byzantineNodes)

  def addEvent(content: T): HashDAG[T] =
    // generate the event
    val currentHeads = getCurrentHeads
    val event        = generator(content)

    // apply the event
    effector(event)

  def generateDelta(content: T): HashDAG[T] =
    // generate the event
    val currentHeads = getCurrentHeads
    val event        = generator(content)

    // apply the event
    this.empty.effector(event)

  def processQueue(): HashDAG[T] =
    var hashDAG = this
    var events  = Set.empty[Event[T]]

    for event <- this.queue do {
      val tmp = hashDAG.effector(event)
      hashDAG = tmp
    }

    hashDAG

  def getEventByID(id: String): Event[T] =
    events(id)

  def pathExists(from: String, to: String, visited: Set[String] = Set()): Boolean =
    def dfs(current: String, visited: Set[String]): Boolean =
      if current == to then true
      else if visited.contains(current) then false
      else
        val neighbours = graph.getOrElse(current, Nil)
        neighbours.exists(neighbor => dfs(neighbor, visited + current))

    dfs(from, Set())

  def isByzantine(event: Event[T]): Boolean =
    if byzantineNodes.contains(event.author) then
      true
    else
      // detect equivocation
      var isByz = false
      for otherEventId <- graph.keys do {
        val otherEvent = events(otherEventId)
        if otherEvent.author == event.author && event.dependencies.intersect(otherEvent.dependencies).nonEmpty then
          isByz = true
      }

      isByz

  def autohrIsByzantine(author: PublicKey): Boolean =
    byzantineNodes.contains(author)

  /*def markEventsFromByzantineNode(author: PublicKey): HashDAG[T] = {
    var e = this.events

    for event <- e.values do
      if event.author == author then
        e = e + (event.id -> event.copy(authorIsByzantine = true))

    this.copy(events = e, byzantineNodes = this.byzantineNodes + author)
  }*/

  def empty: HashDAG[T] =
    HashDAG(this.publicKey, None)

  def withQueue(events: Set[Event[T]]): HashDAG[T] =
    this.empty.copy(queue = events)

  def getIDs: Set[String] = events.keySet

  def getDelta(ids: List[String]): HashDAG[T] =
    withQueue(ids.map(id => events(id)).toSet)

object HashDAG:
  def apply[T](publicKey: PublicKey, privateKey: Option[PrivateKey]): HashDAG[T] =
    val graph = new HashMap[String, Set[String]]()
    val root  = new Event("0", None, publicKey, Set.empty, Array.empty).asInstanceOf[Event[T]]

    new HashDAG[T](graph.updated(root.id, Set.empty), Map(root.id -> root), publicKey, privateKey)
