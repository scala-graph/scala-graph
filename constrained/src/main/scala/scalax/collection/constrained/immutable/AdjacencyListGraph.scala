package scalax.collection.constrained
package immutable

import scala.language.{higherKinds, postfixOps}
import scala.collection.generic.CanBuildFrom

import scalax.collection.GraphPredef.{EdgeLike, InParam, OuterEdge, OuterNode}
import scalax.collection.immutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}
import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}

import generic.GraphConstrainedCompanion
import config.GenConstrainedConfig
import PreCheckFollowUp._

trait AdjacencyListGraph[
    N, E[X] <: EdgeLike[X], +This[X, Y[X] <: EdgeLike[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends GraphLike[N, E, This]
    with SimpleAdjacencyListGraph[N, E, This] { this: This[N, E] =>
  protected type Config <: GraphConfig with GenConstrainedConfig with AdjacencyListArrayConfig
  override protected def initialize(nodes: Traversable[N], edges: Traversable[E[N]]) {
    withoutChecks { super.initialize(nodes, edges) }
  }

  /** generic constrained addition */
  protected def checkedAdd[G >: This[N, E]](contained: => Boolean,
                                            preAdd: => PreCheckResult,
                                            copy: => G,
                                            nodes: => Traversable[N],
                                            edges: => Traversable[E[N]]): This[N, E] =
    if (checkSuspended)
      copy.asInstanceOf[This[N, E]]
    else {
      var graph = this
      if (!contained) {
        var handle         = false
        val preCheckResult = preAdd
        preCheckResult.followUp match {
          case Complete => graph = copy.asInstanceOf[This[N, E]]
          case PostCheck =>
            graph = copy.asInstanceOf[This[N, E]]
            if (!postAdd(graph, nodes, edges, preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort => handle = true
        }
        if (handle) onAdditionRefused(nodes, edges, this)
      }
      graph
    }

  override def +(node: N) =
    checkedAdd(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = copy(nodes.toOuter.toBuffer += node, edges.toOuter),
      nodes = Set(node),
      edges = Set.empty[E[N]])

  override protected def +#(edge: E[N]) =
    checkedAdd(
      contained = edges contains InnerEdge(edge),
      preAdd = preAdd(edge),
      copy = copy(nodes.toOuter, edges.toOuter.toBuffer += edge),
      nodes = Set.empty[N],
      edges = Set(edge))

  /** generic constrained subtraction of nodes */
  protected def checkedSubtractNode[G >: This[N, E]](node: N, forced: Boolean, copy: (N, NodeT) => G): This[N, E] =
    nodes find node map { innerNode =>
      def subtract = copy(node, innerNode).asInstanceOf[This[N, E]]
      if (checkSuspended)
        copy(node, innerNode).asInstanceOf[This[N, E]]
      else {
        var graph          = this
        var handle         = false
        val preCheckResult = preSubtract(innerNode.asInstanceOf[self.NodeT], forced)
        preCheckResult.followUp match {
          case Complete => graph = subtract
          case PostCheck =>
            graph = subtract
            if (!postSubtract(graph, Set(node), Set.empty[E[N]], preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort => handle = true
        }
        if (handle) onSubtractionRefused(Set(innerNode.asInstanceOf[Graph[N, E]#NodeT]), Set.empty[self.EdgeT], graph)
        graph
      }
    } getOrElse this

  override def -(n: N) = checkedSubtractNode(
    n,
    true,
    (outeNode: N, innerNode: NodeT) =>
      copy(nodes.toOuter.toBuffer -= outeNode, edges.toOuter.toBuffer --= (innerNode.edges map (_.toOuter))))

  /** generic constrained subtraction of edges */
  protected def checkedSubtractEdge[G >: This[N, E]](edge: E[N],
                                                     simple: Boolean,
                                                     copy: (E[N], EdgeT) => G): This[N, E] =
    edges find edge map { innerEdge =>
      def subtract = copy(edge, innerEdge).asInstanceOf[This[N, E]]
      if (checkSuspended) subtract
      else {
        var graph          = this
        var handle         = false
        val preCheckResult = preSubtract(innerEdge.asInstanceOf[self.EdgeT], simple)
        preCheckResult.followUp match {
          case Complete => graph = subtract
          case PostCheck =>
            graph = subtract
            if (!postSubtract(graph, Set.empty[N], Set(edge), preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort => handle = true
        }
        if (handle) onSubtractionRefused(Set.empty[self.NodeT], Set(innerEdge.asInstanceOf[self.EdgeT]), graph)
        graph
      }
    } getOrElse this

  override protected def -!(e: E[N]) = checkedSubtractEdge(
    e,
    false,
    (outerEdge: E[N], innerEdge: EdgeT) =>
      copy(nodes.toOuter.toBuffer --= innerEdge.privateNodes map (n => n.value), edges.toOuter.toBuffer -= e))
}
