package scalax.collection.constrained
package constraints

import scala.language.{higherKinds, postfixOps}
import scala.collection.Set

import scalax.collection.GraphPredef._
import scalax.collection.{Graph => SimpleGraph}
import scalax.collection.GraphTraversal.AnyConnected

import PreCheckFollowUp._

/** Ensures that the underlying `Graph` is connected if it is undirected
  * or weakly connected if it is directed.
  */
class Connected[N, E <: EdgeLike[N]](override val self: Graph[N, E]) extends Constraint[N, E](self) {

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E]) =
    PreCheckResult(
      if (edges.isEmpty && nodes.size <= 1 ||
          nodes.isEmpty && edges.size <= 1) Complete
      else PostCheck
    )

  /** `Complete` if `node` is contained even though no addition will be performed;
    *  otherwise `Abort` because `node` would become isolated. */
  override def preAdd(node: N) = PreCheckResult.complete(self contains node)

  /** `Complete` if `edge` itself or at least one end of `edge` is already contained;
    *  otherwise `Abort`. */
  override def preAdd(edge: E) = PreCheckResult.complete(
    (self contains (edge.asInstanceOf[OuterEdge[N, E]])) ||
      (edge exists (self contains _))
  )

  /** `Complete` if `elems` build a connected graph and at least one node of `elems`
    *  is already contained; otherwise `Abort`. */
  override def preAdd(elems: InParam[N, E]*) = PreCheckResult.complete {
    val p        = Param.Partitions(elems)
    val graphAdd = SimpleGraph.from(p.toOuterNodes, p.toOuterEdges)(self.edgeT)
    graphAdd.isConnected &&
    (self.isEmpty ||
    (graphAdd.nodes exists (self find _ isDefined)))
  }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E],
                       preCheck: PreCheckResult) = newGraph.isConnected

  /** Checks within any `preSubtract` whether the neighborhood of the elements
    * to be subtracted remains connected after the subtraction thus preventing
    * a full traversal of the graph.
    *
    * @param include nodes in the neighborhood of the nodes/edges to be subtracted.
    * @param excludeNodes nodes to be subtracted.
    * @param excludeEdges edges to be subtracted.
    * @return `true`if all nodes in `include` are connected.
    */
  protected def isConnected(include: Set[self.NodeT], excludeNodes: Set[self.NodeT], excludeEdges: Set[self.EdgeT]) =
    include.headOption map { head =>
      val cnt = head
        .withDirection(AnyConnected)
        .withSubgraph(
          nodes = n => (include contains n) && !(excludeNodes contains n),
          edges = e => !(excludeEdges contains e)
        )
        .size
      cnt == include.size
    } getOrElse true

  override def preSubtract(node: self.NodeT, forced: Boolean) =
    PreCheckResult.complete(isConnected(node.neighbors, Set(node), node.edges.toSet))

  override def preSubtract(edge: self.EdgeT, simple: Boolean) =
    PreCheckResult.complete(
      if (simple) isConnected(edge.ends.toSet, Set.empty, Set(edge))
      else isConnected(edge.ends.toSet -- edge.privateNodes, edge.privateNodes, Set(edge))
    )
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean) =
    PreCheckResult.complete({
      def neighbors(nodes: Set[self.NodeT]) =
        (for (n <- nodes) yield n.neighbors).flatten
      val nodesToInspect = nodes ++ (for {
        e <- edges
        n <- e
      } yield n)
      if (simple)
        isConnected(neighbors(nodesToInspect) -- nodes, nodes, edges ++ (for (n <- nodes) yield n.edges).flatten)
      else
        isConnected(
          neighbors(nodesToInspect) -- nodes,
          nodes ++ (for (e <- edges) yield e.privateNodes).flatten,
          edges ++ (for (n <- nodes) yield n.edges).flatten)
    })
}

object Connected extends ConstraintCompanion[Connected] {
  def apply[N, E <: EdgeLike[N]](self: Graph[N, E]) = new Connected[N, E](self)
}
