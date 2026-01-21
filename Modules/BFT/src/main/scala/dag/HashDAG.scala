package dag

import scala.collection.immutable.{HashMap, List, Map, Set}
import java.security.{PrivateKey, PublicKey}
import crypto.Ed25519Util

import scala.annotation.tailrec
import scala.collection.mutable

// a hash directed acyclic graph
case class HashDAG[T](
    graph: Map[String, Set[String]],
    events: Map[String, Event[T]],
    publicKey: PublicKey,
    privateKey: Option[PrivateKey],
    queue: Set[Event[T]] = Set.empty[Event[T]], // Map[Event[T], Int] = Map.empty[Event[T], Int],
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

    def generate(content: T): Event[T] = {
      val signature = Ed25519Util.sign(content.toString.getBytes, privateKey.get)

      Event(Some(content), publicKey, getCurrentHeadsIDs, signature)
    }

    def merge(delta: HashDAG[T]): HashDAG[T] =
        var result = this

        for event <- delta.getEventsTopologicallySorted ++ delta.queue do
            result = result.effect(event)

        // This works, a bit slow, but it works
        var i = result.queue.size
        while i != 0 do
            i = 0
            for event <- result.queue do
                result = result.effect(event)
                if result.contains(event) then i += 1

        result

        // collect ready events (inDeg == 0)
        /*val ready = mutable.Queue.empty[Event[T]]
        result.queue.foreach { (e, inDeg)  =>
          if inDeg == 0 then ready.enqueue(e)
        }

        // process ready events
        while ready.nonEmpty do
          val e = ready.dequeue()

          val before = result
          val after = result.effector(e)

          if (after ne before) then {
            result = after

            ready.enqueueAll(result.queue.filter((e, i) => i == 0).keySet)
          }

        result*/

    def effect(event: Event[T]): HashDAG[T] =
      if contains(event) then
          this
      else
          if event.calculateHash != event.id || !event.signatureIsValid then
              this
          /*else if isByzantine(event) then
              // markEventsFromByzantineNode(event.author)

              this.copy(byzantineNodes = this.byzantineNodes + event.author)*/
          else
              val dependencies = event.dependencies
              if dependencies.forall(e => contains(e)) then
                  var g = this.graph
                  for d <- dependencies do
                      val e = events(d)
                      g = g.updated(d, g(d) + event.id)

                  /*val tmp = this.queue.keySet.filter(e => e.dependencies.contains(event.id))
                  var q = this.queue
                  for t <- tmp do
                    q = q + (t -> (q(t) - 1))*/

                  HashDAG(
                    g + (event.id           -> Set.empty),
                    this.events + (event.id -> event),
                    this.publicKey,
                    this.privateKey,
                    this.queue - event,
                  )
              else
                  HashDAG(
                    this.graph,
                    this.events,
                    this.publicKey,
                    this.privateKey,
                    this.queue + event, // (event -> event.dependencies.count(d => this.contains(d))),
                  )

    def addEvent(content: T): HashDAG[T] =
        // generate the event
        val currentHeads = getCurrentHeads
        val event        = generate(content)

        // apply the event
        effect(event)

    def generateDelta(content: T): HashDAG[T] = {
      // private key is empty => cannot sign any events => cannot generate any new events
      // usually the private key is empty in the case of a delta, because before sending a delta we empty the private
      // key to prevent leaking it out
      if privateKey.isEmpty then
          this
      else
          // generate the event
          val currentHeads = getCurrentHeads
          val event        = generate(content)

          // apply the event
          this.empty.effect(event)
    }

    def processQueue(): HashDAG[T] =
        var hashDAG = this
        var events  = Set.empty[Event[T]]

        for event <- this.queue do {
          val tmp = hashDAG.effect(event)
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

    /*def isByzantine(event: Event[T]): Boolean =
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
      byzantineNodes.contains(author)*/

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
      this.empty.copy(queue = events) // .map(e => e -> 0).toMap)

    def getIDs: Set[String] = events.keySet ++ queue.map(e => e.id)

    def getDelta(ids: List[String]): HashDAG[T] =
      withQueue(ids.map(id => events(id)).toSet)

    def orderEvents(events: Iterable[Event[T]]): List[Event[T]] =
      events.toList.sortBy(getEventsTopologicallySorted.zipWithIndex.toMap)

    def getEventsTopologicallySorted: List[Event[T]] =
        val nodes = graph.keySet ++ graph.values.flatten

        // Compute in-degree of each node
        val inDegree = graph.values.flatten
          .groupBy(identity)
          .view.mapValues(_.size)
          .toMap
          .withDefaultValue(0)

        @tailrec
        def loop(queue: List[String], inDeg: Map[String, Int], acc: List[String]): List[String] = queue match
            case Nil          => acc.reverse
            case node :: rest =>
              val children = graph.getOrElse(node, Set.empty)
              // update in-degree
              val updatedInDeg = children.foldLeft(inDeg) { (m, c) => m.updated(c, m(c) - 1) }
              // find nodes that have in-degree 0
              val nodesWithInDegree0 = children.filter(c => updatedInDeg(c) == 0).toList

              loop(rest ++ nodesWithInDegree0, updatedInDeg, node :: acc)

        loop(nodes.filter(n => inDegree(n) == 0).toList, inDegree, Nil).map(id => events(id))

    def getDirectDependencies(id: String): Set[String] =
      if events.contains(id) then
          events(id).dependencies
      else
          Set.empty

    def getNDependencies(id: String, n: Int): Set[String] =

        @tailrec
        def loop(curr: Set[String], depth: Int, visited: Set[String]): Set[String] =
          if depth > n || curr.isEmpty then
              visited
          else {
            val next = curr.flatMap(dep => events(dep).dependencies)
            loop(next, depth + 1, visited ++ curr)
          }

        loop(events(id).dependencies, 1, Set.empty)

    def getAllSuccessors(id: String): Set[String] =
        var visited = Set.empty[String]
        var stack   = mutable.Stack(id)

        while stack.nonEmpty do
            val v = stack.pop()
            if !visited.contains(v) then
                visited = visited + v
                stack.pushAll(graph(v))

        visited - id

object HashDAG:
    def apply[T](publicKey: PublicKey, privateKey: Option[PrivateKey]): HashDAG[T] =
        val graph = new HashMap[String, Set[String]]()
        val root  = new Event("0", None, publicKey, Set.empty, Array.empty).asInstanceOf[Event[T]]

        new HashDAG[T](graph.updated(root.id, Set.empty), Map(root.id -> root), publicKey, privateKey)
