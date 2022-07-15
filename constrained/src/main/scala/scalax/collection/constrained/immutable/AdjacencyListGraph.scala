package scalax.collection.constrained
package immutable

import scalax.collection.GraphPredef.{Edge, InParam, OuterEdge, OuterNode}
import scalax.collection.immutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}
import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}
import config.GenConstrainedConfig

trait AdjacencyListGraph[
    N, E <: Edge[N], +CC[X, Y <: Edge[N]] <: AdjacencyListGraph[X, Y, CC] with Graph[X, Y]]
    extends SimpleAdjacencyListGraph[N, E, CC]
    with GraphLike[N, E, CC] { this: CC[N, E] =>

  protected type Config <: GraphConfig with GenConstrainedConfig with AdjacencyListArrayConfig

  final override protected def initialize(nodes: Iterable[N], edges: Iterable[E]) {
    withoutChecks { super.initialize(nodes, edges) }
  }

  def copy_?(nodes: Iterable[N], edges: Iterable[E]): Either[ConstraintViolation, This[N, E]]

  final override def +(node: N): CC[N, E] = +?(node) getOrElse this

  final def +?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = copy(nodes.outerIterable ++ Iterable(node), edges.outerIterable),
      nodes = Set(node),
      edges = Set.empty[E])

  final override protected def +#(e: E): CC[N, E] = +#?(e) getOrElse this

  final protected def +#?(e: E): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = edges contains Edge(e),
      preAdd = preAdd(e),
      copy = copy(nodes.outerIterable, edges.outerIterable ++ Iterable(e)),
      nodes = Set.empty[N],
      edges = Set(e))

  final override def -(node: N): CC[N, E] = -?(node) getOrElse this

  final def -?(n: N): Either[ConstraintViolation, This[N, E]] = checkedMinusNode(
    n,
    forced = true,
    (outerNode: N, innerNode: NodeT) => {
      val exclNodes = innerNode.edges map (_.outer)
      copy(
        nodes.outerIterable.filterNot(_ == outerNode),
        edges.outerIterable.filterNot(exclNodes.contains)
      )
    }
  )

  final override def minusIsolated(n: N): CC[N, E] = minusIsolated_?(n) getOrElse this

  final def minusIsolated_?(n: N): Either[ConstraintViolation, This[N, E]] = checkedMinusNode(
    n,
    true,
    (outeNode: N, innerNode: NodeT) => {
      val exclNodes = innerNode.edges map (_.outer)
      copy(
        nodes.outerIterable.filterNot(_ == outerNode),
        edges.outerIterable.filterNot(exclNodes.contains))
      )
    }
  )

  /** generic constrained subtraction of edges */
  protected def checkedSubtractEdge[G >: CC[N, E]](edge: E,
                                                     simple: Boolean,
                                                     copy: (E, EdgeT) => G): CC[N, E] =
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
    }
  )

  final override protected def -#(e: E): CC[N, E] = -#?(e) getOrElse this

  final protected def -#?(e: E): Either[ConstraintViolation, This[N, E]] = checkedMinusEdge(
    e,
    simple = true,
    (outerEdge: E, innerEdge: EdgeT) => copy(nodes.outerIterable, edges.outerIterable.filterNot(_ == outerEdge))

  final override protected def -!#(e: E): CC[N, E] = -!#?(e) getOrElse this

  final protected def -!#?(e: E): Either[ConstraintViolation, This[N, E]] = checkedMinusEdge(
    e,
    simple = false,
    (outerEdge: E, innerEdge: EdgeT) => {
      val exclNodes = innerEdge.privateNodes map (_.outer)
      copy(
        nodes.outerIterable.filterNot(exclNodes.contains),
        edges.outerIterable.filterNot(_ == e))
    }
  )
}
