package dag

import scala.collection.immutable.{HashMap, List, Map, Set}
import java.security.{KeyPair, PublicKey}
import crypto.Ed25519Util
import riblt.{CodedSymbol, RIBLT, given_Hashable_String, given_Xorable_String}

// a hash directed acyclic graph
case class HashDAG[T] private (
    // the state S of a HashDAG consists of the vertices and edges contained in the HashDAG, S = (V, E)
    // but separately saving the vertices and edges is redundant,
    // a better way to solve this is by mapping each vertex to the set of its children
    graph: Map[Event[T], Set[String]],
    authorKeys: KeyPair,
    queue: Set[Event[T]] = Set.empty[Event[T]],
    byzantineNodes: Set[PublicKey] = Set.empty,
    riblt: RIBLT[String] = RIBLT()
):

  // checks if an event is contained in the graph
  def contains(event: Event[T]): Boolean = graph.contains(event)

  def contains(id: String): Boolean =
    graph.exists((e, _) => e.id == id)

  def contains(node: PublicKey): Boolean =
    graph.exists((e, _) => e.author == node)

  // checks for the existence of child vertices
  def hasChild(event: Event[T]): Boolean =
    assume(contains(event), "The given node is not part of the HashDAG!")

    graph(event).nonEmpty

  // returns the current forward extremities (aka Heads)
  private def getCurrentHeads: Set[Event[T]] =
    Set.from(graph.filter((_, set) => set.isEmpty).map((event, _) => event))

  def generator(content: T): Event[T] = {
    val currentHeads    = getCurrentHeads
    val currentHeadsIDs = currentHeads.map(event => event.id)
    val signature       = Ed25519Util.sign(content.toString.getBytes, authorKeys.getPrivate)

    Event(Some(content), authorKeys.getPublic, currentHeadsIDs, signature)
  }

  def effector(event: Event[T]): HashDAG[T] =
    if contains(event) then
      this
    else
      if event.calculateHash != event.id || !event.signatureIsValid || event.authorIsByzantine then
        this
      else if isByzantine(event) then
        markEventsFromByzantineNode(event.author)
      else
        val dependencies = event.dependencies
        if dependencies.forall(e => contains(e)) then
          var g = this.graph
          for d <- dependencies do
            val e = g.find((e, _) => e.id == d).get._1
            g = g.updated(e, g(e) + event.id)

          this.riblt.addSymbol(event.id)

          //this.riblt_enc.restart

          HashDAG(
            g + (event -> Set.empty),
            this.authorKeys,
            this.queue - event,
            this.byzantineNodes,
            this.riblt
          )
        else
          HashDAG(this.graph, this.authorKeys, this.queue + event, this.byzantineNodes, this.riblt)

  def addEvent(content: T): HashDAG[T] =
    // generate the event
    val currentHeads = getCurrentHeads
    val event        = generator(content)

    // apply the event
    effector(event)

  def processQueue(): HashDAG[T] =
    var hashDAG = this
    var events  = Set.empty[Event[T]]

    for event <- this.queue do {
      val tmp = hashDAG.effector(event)
      hashDAG = tmp
    }

    hashDAG

  def getEventByID(id: String): Event[T] =
    graph.find((e, _) => e.id == id).get._1

  def pathExists(from: String, to: String, visited: Set[String] = Set()): Boolean =
    def dfs(current: String, visited: Set[String]): Boolean =
      if current == to then true
      else if visited.contains(current) then false
      else
        val neighbours = graph.getOrElse(getEventByID(current), Nil)
        neighbours.exists(neighbor => dfs(neighbor, visited + current))

    dfs(from, Set())

  def isByzantine(event: Event[T]): Boolean =
    if byzantineNodes.contains(event.author) then
      true
    else
      // detect equivocation
      var isByz = false
      for otherEvent <- graph.keys do
        if otherEvent.author == event.author && event.dependencies.intersect(otherEvent.dependencies).nonEmpty then
          isByz = true

      isByz

  def markEventsFromByzantineNode(author: PublicKey): HashDAG[T] = {
    var g = this.graph

    for event <- graph.keys do
      if event.author == author then
        val v = graph(event)
        g = g - event
        g = g + (event.copy(authorIsByzantine = true) -> v)

    HashDAG(g, this.authorKeys, this.queue, this.byzantineNodes + author, this.riblt)
  }

  def produceNextCodedSymbols(count: Int = 1): List[CodedSymbol[String]] =
    var codedSymbols = List.empty[CodedSymbol[String]]

    for (i <- 0 to count)
      codedSymbols = codedSymbols :+ riblt.produceNextCodedSymbol

    codedSymbols

  def addCodedSymbols(codedSymbols: List[CodedSymbol[String]]): (HashDAG[T], Boolean) =
    if riblt.codedSymbols.nonEmpty && riblt.isDecoded then
      (this, true)
    else
      for codedSymbol <- codedSymbols do
        riblt.addCodedSymbol(codedSymbol)

      riblt.tryDecode
      (this.copy(riblt = this.riblt), riblt.isDecoded)

  def sendDiff: (Set[Event[T]], Set[String]) =
    if riblt.isDecoded then
      val ids = riblt.localSymbols.map(s => s.symbol)
      (
        graph.filter((k, v) => ids.contains(k.id)).keySet,
        riblt.remoteSymbols.map(s => s.symbol).toSet
      )
    else
      (Set.empty, Set.empty)

  def receiveDiff(response: Set[Event[T]], request: Set[String]): (HashDAG[T], Set[Event[T]]) =
    var tmp = this

    for e <- response do
      tmp = tmp.effector(e)

    (
      tmp,
      this.graph.filter((k, v) => request.contains(k.id)).keySet
    )

  def decRestart: HashDAG[T] = ???



object HashDAG:
  def apply[T](authorKeys: KeyPair): HashDAG[T] =
    val graph = new HashMap[Event[T], Set[String]]()
    val root  = new Event("0", None, authorKeys.getPublic, Set.empty, Array.empty).asInstanceOf[Event[T]]

    new HashDAG[T](graph.updated(root, Set.empty), authorKeys)
