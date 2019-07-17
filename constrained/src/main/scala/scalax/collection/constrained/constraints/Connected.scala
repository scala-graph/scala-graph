package scalax.collection.constrained
package constraints

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, postfixOps}
import scala.collection.Set

import scalax.collection.GraphPredef._
import scalax.collection.{Graph => SimpleGraph}
import scalax.collection.GraphTraversal.AnyConnected

import PreCheckFollowUp._

/** Ensures that the underlying `Graph` is connected if it is undirected
  * or weakly connected if it is directed.
  */
class Connected[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G) extends Constraint[N, E, G](self) {

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]) =
    PreCheckResult(
      if (edges.isEmpty && nodes.size <= 1 ||
          nodes.isEmpty && edges.size <= 1) Complete
      else PostCheck
    )

  /** `Complete` if `node` is contained even though no addition will be performed;
    *  otherwise `Abort` because `node` would become isolated. */
  override def preAdd(node: N): PreCheckResult = PreCheckResult.complete(self contains node)

  /** `Complete` if `edge` itself or at least one end of `edge` is already contained;
    *  otherwise `Abort`. */
  override def preAdd(edge: E[N]): PreCheckResult = PreCheckResult.complete(
    (self contains edge.asInstanceOf[OuterEdge[N, E]]) ||
      (edge exists (self contains _))
  )

  /** `Complete` if `elems` build a connected graph and at least one node of `elems`
    *  is already contained; otherwise `Abort`. */
  override def preAdd(elems: InParam[N, E]*): PreCheckResult = PreCheckResult.complete {
    val p        = Param.Partitions(elems)
    val graphAdd = SimpleGraph.from(p.toOuterNodes, p.toOuterEdges)(self.edgeT)
    graphAdd.isConnected &&
    (self.isEmpty ||
    (graphAdd.nodes exists (self find _ isDefined)))
  }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: G @uV,
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Either[PostCheckFailure, G] =
    if (newGraph.isConnected) Right(newGraph)
    else Left(PostCheckFailure(s"Unexpected isolated node found when adding $passedNodes, $passedEdges."))

  /** Checks within any `preSubtract` whether the neighborhood of the elements
    * to be subtracted remains connected after the subtraction thus preventing
    * a full traversal of the graph.
    *
    * @param include nodes in the neighborhood of the nodes/edges to be subtracted.
    * @param excludeNodes nodes to be subtracted.
    * @param excludeEdges edges to be subtracted.
    * @return `true`if all nodes in `include` are connected.
    */
  protected def isConnected(include: Set[self.NodeT],
                            excludeNodes: Set[self.NodeT],
                            excludeEdges: Set[self.EdgeT]): Boolean =
    include.headOption forall { head =>
      val cnt = head
        .withDirection(AnyConnected)
        .withSubgraph(
          nodes = n => (include contains n) && !(excludeNodes contains n),
          edges = e => !(excludeEdges contains e)
        )
        .size
      cnt == include.size
    }

  override def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult =
    PreCheckResult.complete(isConnected(node.neighbors, Set(node), node.edges.toSet))

  override def preSubtract(edge: self.EdgeT, simple: Boolean): PreCheckResult =
    PreCheckResult.complete(
      if (simple) isConnected(edge.nodes.toSet, Set.empty, Set(edge))
      else isConnected(edge.nodes.toSet -- edge.privateNodes, edge.privateNodes, Set(edge))
    )
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
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
  def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new Connected[N, E, G](self)
}
