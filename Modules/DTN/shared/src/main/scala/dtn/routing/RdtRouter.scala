package dtn.routing

import dtn.{DtnPeer, Endpoint, MonitoringClientInterface, NoMonitoringClient, Packet, PreviousNodeBlock, RdtMessageType, RdtMetaBlock, RdtMetaInfo, Sender, WSEroutingClient}
import rdts.time.Dots

import java.time.ZonedDateTime
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.Random

/*
  This router only routes rdt-bundles. Other bundles are currently ignored.

  There are two types of rdt-bundles:
    1. Request-Bundles, which only contains a dot-set that represents the known state and thereby requests anything unknown.
    2. Payload-Bundles, which contain a dot-set and a payload, where the dot-set represents everything included in the payload.

  Request-Bundle routing:
    We use epidemic routing here, as the dot-set without a payload does not contribute to our known state that we can intelligently route.
    Also, the requests should reach everybody as fast as possible with as few messages as possible, because we do not know who is contributing to our rdt.

  Payload-Bundle routing:
    We need to determine two things in this router for a given payload-bundle:

    1. to which destinations must we forward this bundle?

        what do we do:

          To be able to merge source-node-information, the bundle-source will be a unicast endpoint like: dtn://node-id/rdt/app-name
          In contrast to the bundle-destination, which will be a group endpoint like: dtn://global/~rdt/app-name

          this means that each rdt-app must subscribe to two endpoints.
          currently bundles will be only addressed to the global one, but this may change.

          we have a map(~crdt-group-endpoint -> map(dtn://node-id -> Dots))

          we store the dot-set for each endpoint, grouped by crdt for faster processing

          on each incoming bundle with a dot-set we temporarily store this dot-set until a forward-request is issued

          on each forward-request we use our temporarily stored dot-set (if available) to compare it to the other known dot-sets in our store,
          thereby determining the nodes to forward our bundle to

    2. which neighbour is our best choice to forward this bundle over for a given destination?

        what do we do:

          we store a score for each destination node per neighbour

          the higher the score, the more likely it is that the bundle will reach the destination node via this neighbour

          structure is like: Map[destination_node: Endpoint, Map[neighbour_node: Endpoint, score: Long]]

          each received bundle from a neighbour:
            1. increases the score for the destination=bundle-source-node and neighbour=bundle-previous-node by 1
            2. normalizes scores over all combinations with that destination

          on a forward request, the sorted list with the highest score first is returned. the first n neighbours are picked.
 */

class RdtRouter(
    ws: WSEroutingClient,
    monitoringClient: MonitoringClientInterface,
    nTotalNodes: Int,
    topNNeighbours: Int
) extends BaseRouter(ws: WSEroutingClient, monitoringClient: MonitoringClientInterface) {
  // epidemic routing is for rdt-request-bundles. they do not contain any payload that contributes to our state (in contrast to rdt-payload-bundles)
  val epidemicStrategy: EpidemicStrategy = EpidemicStrategy()

  // state will grow indefinitely. there is currently no garbage collector
  val likelihoodState: LikelihoodState = LikelihoodState()
  val dotState: DotState               = DotState()

  // maybe grows indefinitely, but only if we receive a bundle which will not be forwarded (hop count depleted?, local unicast endpoint?)
  val tempRdtMetaInfoStore: ConcurrentHashMap[String, RdtMetaInfo] = ConcurrentHashMap()
  val tempRdtIdStore: ConcurrentHashMap[String, String]            = ConcurrentHashMap()

  val delivered: ConcurrentHashMap[String, Int] =
    ConcurrentHashMap() // shows the number of nodes we have delivered a bundle to

  override def onRequestSenderForBundle(packet: Packet.RequestSenderForBundle)
      : Option[Packet.ResponseSenderForBundle] = {
    println(s"received sender-request for bundle: ${packet.bp}")

    // we only route rdt packets rn
    if !tempRdtMetaInfoStore.containsKey(packet.bp.id) then {
      println(s"bundle meta information for bundle-id ${packet.bp.id} are not available. not routing rn.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    val source_node: Endpoint      = packet.bp.source.extract_node_endpoint()
    val rdt_id: String             = packet.bp.destination.extract_endpoint_id()
    val rdt_meta_info: RdtMetaInfo = tempRdtMetaInfoStore.get(packet.bp.id)

    rdt_meta_info.message_type match
      case RdtMessageType.Request => {
        println("got rdt request bundle. routing with epidemic strategy")
        return epidemicStrategy.onRequestSenderForBundle(peers, services, packet)
      }
      case RdtMessageType.Payload => {
        println("got rdt payload bundle. routing with rdt strategy")
      }

    // if we already successfully forwarded this package to enough clas we can 'safely' delete it.
    if delivered.getOrDefault(packet.bp.id, 0) >= nTotalNodes then {
      println("bundle was forwarded to enough unique neighbours. deleting.")
      tempRdtMetaInfoStore.remove(packet.bp.id)
      tempRdtIdStore.remove(packet.bp.id)
      return Option(Packet.ResponseSenderForBundle(
        bp = packet.bp,
        clas = List(),
        delete_afterwards = false
      )) // test: false means we keep it in case somebody sends it to us again, which means the routing wont be bothered again.
    }

    // SHORT-CUT: on no known peers return an empty forwarding request
    if peers.isEmpty then {
      println("no known peers. returning early with empty response. keeping bundle.")
      return Option(Packet.ResponseSenderForBundle(bp = packet.bp, clas = List(), delete_afterwards = false))
    }

    // get all destination nodes to which we must forward this bundle
    val destination_nodes: Set[Endpoint] =
      dotState.getDestinationNodeEndpoints(source_node, rdt_id, rdt_meta_info.dots)
    println(s"destination nodes: $destination_nodes")

    // for these destination nodes select the best neighbours to forward this bundle to
    var best_neighbours =
      destination_nodes.flatMap(node => likelihoodState.get_sorted_neighbours(node).take(topNNeighbours))
    println(s"best neighbours: $best_neighbours")

    // remove neighbours which already know the state
    best_neighbours = dotState.filterNeighbourNodes(best_neighbours, rdt_id, rdt_meta_info.dots)
    println(s"best neighbours without neighbours that already know the state: $best_neighbours")

    // use current-peers-list to try and get peer information for each best neighbour
    var targets: List[DtnPeer] = best_neighbours
      .map(x => peers.get(x.extract_node_name()))
      .filter(x => x != null)
      .toList
    println(s"multicast targets: $targets")

    if targets.size < topNNeighbours then {
      println(s"multicast targets are less than ${topNNeighbours}, trying to add random peers from fallback strategy")

      var random_peers: Iterable[DtnPeer] = peers.values().asScala

      // remove peers which already know the state
      random_peers = dotState.filterPeers(random_peers, rdt_id, rdt_meta_info.dots)

      // remove peers that are already in the multicast selection
      random_peers = random_peers.filter(p => !targets.contains(p))

      println(s"filtered random peers available: ${List.from(random_peers).map(peer => peer.eid)}")

      targets = targets ++ Random.shuffle(random_peers).take(topNNeighbours - targets.size)
    }

    // use peer-info and available clas' to build a list of cla-connections to forward the bundle over
    val selected_clas: List[Sender] = targets.flatMap(target => {
      target.cla_list
        .filter((agent, port_option) => packet.clas.contains(agent))
        .map((agent, port_option) =>
          Sender(remote = target.addr, port = port_option, agent = agent, next_hop = target.eid)
        )
    })
    println(s"selected clas: $selected_clas")

    println(s"time: ${ZonedDateTime.now()}")

    return Option(Packet.ResponseSenderForBundle(
      bp = packet.bp,
      clas = selected_clas,
      delete_afterwards = false
    ))
  }

  override def onError(packet: Packet.Error): Unit = {
    println(s"received error from dtnd: ${packet.reason}")
  }

  override def onTimeout(packet: Packet.Timeout): Unit = {
    println(s"sending ran into timeout for bundle-forward-response ${packet.bp.id}.")
  }

  override def onSendingFailed(packet: Packet.SendingFailed): Unit = {
    println(s"sending failed for bundle ${packet.bid} on cla ${packet.cla_sender}.")
  }

  override def onSendingSucceeded(packet: Packet.SendingSucceeded): Unit = {
    val rdt_meta_info = tempRdtMetaInfoStore.get(packet.bid)
    val rdt_id        = tempRdtIdStore.get(packet.bid)

    print(s"sending succeeded for bundle ${packet.bid} on cla ${packet.cla_sender}. ")

    if rdt_meta_info == null || rdt_id == null then {
      println("no rdt-meta-information are available. ignoring")
    } else {
      rdt_meta_info.message_type match
        case RdtMessageType.Request => {
          println("rdt-request-message. feeding epidemic strat")
          epidemicStrategy.onSendingSucceeded(packet)
        }
        case RdtMessageType.Payload => {
          delivered.merge(packet.bid, 1, (x1, x2) => x1 + x2)
          println("rdt-payload-message. merging dots for next hop")
          dotState.mergeDots(Endpoint.createFromName(packet.cla_sender), rdt_id, rdt_meta_info.dots)
        }

    }
  }

  override def onIncomingBundle(packet: Packet.IncomingBundle): Unit = {
    // we always merge the bundle information, as we might receive it from multiple nodes, which is valuable information to us

    val source_node                        = packet.bndl.primary_block.source.extract_node_endpoint()
    val rdt_id                             = packet.bndl.primary_block.destination.extract_endpoint_id()
    var previous_node: Option[Endpoint]    = None
    var rdt_meta_info: Option[RdtMetaInfo] = None

    packet.bndl.other_blocks.collectFirst {
      case x: PreviousNodeBlock => x
    } match {
      case None                      => println("received incoming bundle without previous node block. ignoring")
      case Some(previous_node_block) => previous_node = Option(previous_node_block.previous_node_id)
    }
    packet.bndl.other_blocks.collectFirst {
      case x: RdtMetaBlock => x
    } match {
      case None                 => println("received incoming bundle without rdt-meta block. ignoring")
      case Some(rdt_meta_block) => rdt_meta_info = Option(rdt_meta_block.info)
    }

    if previous_node.nonEmpty && rdt_meta_info.nonEmpty then {
      println(s"received incoming bundle ${packet.bndl.id} with rdt-meta block and previous-node block. ")

      likelihoodState.update_score(neighbour_node = previous_node.get, destination_node = source_node)

      rdt_meta_info.get.message_type match
        case RdtMessageType.Request => {
          println("rdt-request-message. merging only source")
          dotState.mergeDots(source_node, rdt_id, rdt_meta_info.get.dots)
        }
        case RdtMessageType.Payload => {
          println("rdt-payload-message. merging source and previous node")
          dotState.mergeDots(source_node, rdt_id, rdt_meta_info.get.dots)
          dotState.mergeDots(previous_node.get, rdt_id, rdt_meta_info.get.dots)
        }

      tempRdtMetaInfoStore.put(packet.bndl.id, rdt_meta_info.get)
      tempRdtIdStore.put(packet.bndl.id, rdt_id)
      ()
    }
  }

  override def onIncomingBundleWithoutPreviousNode(packet: Packet.IncomingBundleWithoutPreviousNode): Unit = {
    // todo: if bundle on IncomingBundle is stored temporarily, then use that info to increase the score
    // currently irrelevant, as this message is never sent by the dtnd
    println("received incoming bundle without previous node. information not used for routing. ignoring.")
  }
}
object RdtRouter {
  val N_TOTAL_NODES    = 10
  val TOP_N_NEIGHBOURS = 3

  def apply(
      host: String,
      port: Int,
      monitoringClient: MonitoringClientInterface = NoMonitoringClient,
      nTotalNodes: Int = N_TOTAL_NODES,
      topNNeighbours: Int = TOP_N_NEIGHBOURS,
  ): Future[RdtRouter] =
    WSEroutingClient(host, port).map(ws => new RdtRouter(ws, monitoringClient, nTotalNodes, topNNeighbours))
}

class LikelihoodState {
  // structure: map[destination-node, map[neighbour-node, score]]
  val map: ConcurrentHashMap[Endpoint, Map[Endpoint, Double]] = ConcurrentHashMap()

  /*
    adds 1 to the score of this destination + neighbour combination
    normalizes scores to 1 over all combinations with that destination
   */
  def update_score(destination_node: Endpoint, neighbour_node: Endpoint): Unit = {
    var m = map.getOrDefault(destination_node, Map())

    m = m.updated(neighbour_node, m.getOrElse(neighbour_node, 0.0) + 1.0)

    val sum = m.values.sum

    m = m.map { case (neighbour_node, score) => neighbour_node -> score / sum }

    map.put(destination_node, m)
    ()
  }

  /*
    returns all known neighbours for a destination in a set, sorted by delivery-likelihood, with the best neighbour (highest score) first
   */
  def get_sorted_neighbours(destination_node: Endpoint): Set[Endpoint] = {
    map
      .getOrDefault(destination_node, Map())
      .toList
      .sortBy(-_._2)
      .map(_._1)
      .toSet
  }
}

class DotState {
  // structure: map[rdt-group-id, map[node-id, Dots]]
  val map: ConcurrentHashMap[String, ConcurrentHashMap[Endpoint, Dots]] = ConcurrentHashMap()

  /*
    adds/merges this nodes' dots to the data structure.
   */
  def mergeDots(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Unit = {
    map.putIfAbsent(rdt_id, ConcurrentHashMap())
    map.get(rdt_id).merge(node_endpoint, dots, (x1, x2) => x1.merge(x2))
    ()
  }

  /*
    finds all nodes for which this nodes' dots are bigger than the other nodes' dots
   */
  def getDestinationNodeEndpoints(node_endpoint: Endpoint, rdt_id: String, dots: Dots): Set[Endpoint] = {
    map.get(rdt_id) match
      case null                                       => Set()
      case rdt_map: ConcurrentHashMap[Endpoint, Dots] =>
        rdt_map.asScala
          .filter((n: Endpoint, d: Dots) => !n.equals(node_endpoint) && !(dots <= d))
          .collect[Endpoint]((n: Endpoint, d: Dots) => n)
          .toSet
  }

  /*
    return all neighbours for which the provided dots are not already known to them
   */
  def filterNeighbourNodes(neighbour_node_endpoints: Set[Endpoint], rdt_id: String, dots: Dots): Set[Endpoint] = {
    neighbour_node_endpoints.filter(endpoint => {
      val d = map.get(rdt_id) match
        case null                                        => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(endpoint, Dots.empty)

      !(dots <= d) || dots.isEmpty
    })
  }

  /*
    return all peers for which the provided dots are not already known to them
   */
  def filterPeers(peers: Iterable[DtnPeer], rdt_id: String, dots: Dots): Iterable[DtnPeer] = {
    peers.filter(peer => {
      val d = map.get(rdt_id) match
        case null                                        => Dots.empty
        case node_map: ConcurrentHashMap[Endpoint, Dots] => node_map.getOrDefault(peer.eid, Dots.empty)

      !(dots <= d) || dots.isEmpty
    })
  }
}
