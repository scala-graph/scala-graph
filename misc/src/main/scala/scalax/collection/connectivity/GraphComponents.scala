package scalax.collection.connectivity

import scala.collection.mutable.ListBuffer
import scala.collection.{mutable, Set}
import scala.math.min

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.Graph

import logging.Logging

/**
 * Provides algorithms for finding graph components.
 * @author Vasco Figueira
 */
final class GraphComponents[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N, E])(implicit m: Manifest[N]) extends Logging {

  type DeepSearchStackAggregator = (Seq[g.NodeT]) => Unit

  /**
   * Do Tarjan's algorithm and return the set of sets
   * @return Sets of inner nodes
   */
  def stronglyConnectedSets: Set[Set[N]] = {

    // the result and its builder
    var result: Set[Set[N]] = Set()
    val toValueSet: DeepSearchStackAggregator = result += _.map(_.value).toSet

    // call the algo
    stronglyConnectedComponents(toValueSet)

    result
  }

  /**
   * Do Tarjan's algorithm and return the set of node sets
   * @return Sets of outer nodes
   */
  def stronglyConnectedNodeSets: Set[Set[g.NodeT]] = {

    // the result and its builder
    var result: Set[Set[g.NodeT]] = Set()
    val toSet: DeepSearchStackAggregator = result += _.toSet

    // call the algo
    stronglyConnectedComponents(toSet)

    result
  }

  /**
   * Do Tarjan's algorithm and return the DAG of (cyclic) subgraphs
   * TODO not sound wrt to HyperGraphs
   */
  def stronglyConnectedComponentsDag: Graph[Graph[N, DiEdge], DiEdge] = {

    // the result and its builder
    val lookup: mutable.Map[N, Graph[N, DiEdge]] = mutable.Map()
    var outerEdges: List[DiEdge[Graph[N, DiEdge]]] = List.empty
    var evidence: DiEdge[Graph[N, DiEdge]] = null

    // graph of graphs aggregator
    val toSubgraphDag: DeepSearchStackAggregator = { currentStack =>
      debug("toSubgraphDag - init")
      // previous graphs being referenced by current
      var referencedGraphs: List[Graph[N, DiEdge]] = Nil
      // edges from and to nodes of the current stack
      var innerEdges: List[DiEdge[N]] = Nil

      for (n <- currentStack; e <- n.outgoing) {
        if (e.nodeSeq.forall(currentStack.contains(_))) { // TODO possibly improve performance
          // edge within the subgraph
          innerEdges = (e._1.value ~> e._2.value) :: innerEdges
          debug("%s points to %s within the same subgraph".format(n.value.toString, e._2.value.toString))
        } else {
          // edge of the DAG
          // only points to previous subgraphs
          debug("%s points to %s from another subgraph".format(n.value.toString, e._2.value.toString))
          debug(lookup.mkString(" "))
          referencedGraphs = lookup(e._2.value) :: referencedGraphs
        }
      }
      val theseNodes = currentStack.map(_.value)
      val currentGraph: Graph[N, DiEdge] = Graph.from(theseNodes, innerEdges)
      // create outer edges from current to previous graphs
      evidence = (currentGraph ~> currentGraph)
      outerEdges = referencedGraphs.map((currentGraph ~> _)) ++ outerEdges
      // add currentGraph to lookup by its nodes
      debug("adding " + currentGraph + " to lookup for each node: " + theseNodes)
      lookup ++= theseNodes.map((_ -> currentGraph))
    }

    // call the algo
    stronglyConnectedComponents(toSubgraphDag)
    
    // if(outerEdges.isEmpty) should be able to say something like
    // Graph.from(lookup.values) - ditching evidence and using outerEdges.head

    Graph.from(lookup.values, outerEdges)(Manifest.classType(evidence.getClass))
  }

  /**
   * @param Calls agg for every root node discovered with current node and past stack
   */
  private[this] def stronglyConnectedComponents(aggregator: DeepSearchStackAggregator) {
    // discovery and finish times for each node
    trace("stronglyConnectedComponents: init")
    var time = 0;
    val discovery: mutable.Map[g.NodeT, Int] = mutable.Map()
    val finish: mutable.Map[g.NodeT, Int] = mutable.Map()
    // stack of previous nodes in the search
    var path = ListBuffer.empty[g.NodeT]

    def visit(n: g.NodeT) {
      debug("visiting %c at time %d".format(n.value, time))
      discovery(n) = time
      finish(n) = time
      time += 1
      path. +=: (n)
      for (v <- n.diSuccessors) {
        if (discovery.get(v) == None) {
          visit(v)
          finish(n) = min(finish(v), finish(n))
        } else if (path.contains(v)) {
          // TODO make the contains test O(1)
          trace("updating finish(%c) to %d".format(n.value, min(discovery(v), finish(n))))
          finish(n) = min(discovery(v), finish(n))
        }
      }
      trace("finish(%1$c) == %2$d; discovery(%1$c) == %3$d; path == %s".format(n.value, finish(n), discovery(n), path))
      if (finish(n) == discovery(n)) {
        // n is root, call result aggregator
        var found = 0
        val (scc, rest) = path span { e =>
          if (e == n || found > 0) found += 1
          found <= 1
        }
        debug(n + " is root; its scc is " + scc + "; rest is " + rest)
        aggregator(scc)
        path = rest
      }
    }

    // visit each node not visited yet
    for (n <- g.nodes if (discovery.get(n) == None)) {
      visit(n)
    }
  }

  // reverse will go into Graph core, promise!
}

/**
 * Companion Object
 */
object GraphComponents {

  implicit def graphToComponents[N: Manifest, E[X] <: EdgeLikeIn[X]](g: Graph[N, E]) =
    new GraphComponents[N, E](g)
}

