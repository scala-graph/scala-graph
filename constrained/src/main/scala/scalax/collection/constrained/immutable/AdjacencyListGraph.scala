package scalax.collection.constrained
package immutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, postfixOps}

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}
import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}
import config.GenConstrainedConfig
import PreCheckFollowUp._

trait AdjacencyListGraph[
    N, E[X] <: EdgeLikeIn[X], +This[X, Y[X] <: EdgeLikeIn[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends SimpleAdjacencyListGraph[N, E, This]
    with GraphLike[N, E, This] { this: This[N, E] =>

  protected type Config <: GraphConfig with GenConstrainedConfig with AdjacencyListArrayConfig

  override protected def initialize(nodes: Traversable[N], edges: Traversable[E[N]]) {
    withoutChecks { super.initialize(nodes, edges) }
  }

  /** generic constrained addition */
  protected def checkedAdd(contained: => Boolean,
                           preAdd: => PreCheckResult,
                           copy: => This[N, E] @uV,
                           nodes: => Traversable[N],
                           edges: => Traversable[E[N]]): Either[ConstraintViolation, This[N, E]] =
    if (checkSuspended) Right(copy)
    else if (contained) Right(this)
    else {
      val preCheckResult = preAdd
      preCheckResult.followUp match {
        case Complete  => Right(copy)
        case PostCheck => postAdd(copy, nodes, edges, preCheckResult)
        case Abort     => Left(constraintViolation(preCheckResult))
      }
    }

  def copy_?(nodes: Traversable[N], edges: Traversable[E[N]]): Either[ConstraintViolation, This[N, E]]

  override def +(node: N): This[N, E] = +?(node) getOrElse this

  def +?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedAdd(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = copy(nodes.toOuter.toBuffer += node, edges.toOuter),
      nodes = Set(node),
      edges = Set.empty[E[N]])

  final override protected def +#(e: E[N]): This[N, E] = +#?(e) getOrElse this

  protected def +#?(e: E[N]): Either[ConstraintViolation, This[N, E]] =
    checkedAdd(
      contained = edges contains Edge(e),
      preAdd = preAdd(e),
      copy = copy(nodes.toOuter, edges.toOuter.toBuffer += e),
      nodes = Set.empty[N],
      edges = Set(e))

  /** generic constrained subtraction of nodes */
  protected def checkedSubtractNode(node: N,
                                    forced: Boolean,
                                    copy: (N, NodeT) => This[N, E] @uV): Either[ConstraintViolation, This[N, E]] =
    nodes find node map { innerNode =>
      def subtract = copy(node, innerNode)
      if (checkSuspended)
        Right(copy(node, innerNode))
      else {
        val preCheckResult = preSubtract(innerNode.asInstanceOf[self.NodeT], forced)
        preCheckResult.followUp match {
          case Complete  => Right(subtract)
          case PostCheck => postSubtract(subtract, Set(node), Set.empty[E[N]], preCheckResult)
          case Abort     => Left(constraintViolation(preCheckResult))
        }
      }
    } getOrElse Right(this)

  def -?(n: N): Either[ConstraintViolation, This[N, E]] = checkedSubtractNode(
    n,
    forced = true,
    (outerNode: N, innerNode: NodeT) =>
      copy(nodes.toOuter.toBuffer -= outerNode, edges.toOuter.toBuffer --= (innerNode.edges map (_.toOuter))))

  final override def minusIsolated(n: N): This[N, E] = minusIsolated_?(n) getOrElse this

  def minusIsolated_?(n: N): Either[ConstraintViolation, This[N, E]] = checkedSubtractNode(
    n,
    forced = false,
    (outerNode: N, innerNode: NodeT) => {
      var newNodes = nodes.toOuter.toBuffer
      var newEdges = edges.toOuter.toBuffer
      nodes.subtract(
        innerNode,
        rippleDelete = false,
        innerNode => newNodes -= outerNode,
        innerNode => newEdges --= (innerNode.edges map (_.toOuter)))
      copy(newNodes, newEdges)
    }
  )

  /** generic constrained subtraction of edges */
  protected def checkedSubtractEdge(edge: E[N],
                                    simple: Boolean,
                                    copy: (E[N], EdgeT) => This[N, E] @uV): Either[ConstraintViolation, This[N, E]] =
    edges find edge map { innerEdge =>
      def subtract = copy(edge, innerEdge)
      if (checkSuspended) Right(subtract)
      else {
        val preCheckResult = preSubtract(innerEdge.asInstanceOf[self.EdgeT], simple)
        preCheckResult.followUp match {
          case Complete  => Right(subtract)
          case PostCheck => postSubtract(subtract, Set.empty[N], Set(edge), preCheckResult)
          case Abort     => Left(constraintViolation(preCheckResult))
        }
      }
    } getOrElse Right(this)

  final override protected def -#(e: E[N]): This[N, E] = -#?(e) getOrElse this

  protected def -#?(e: E[N]): Either[ConstraintViolation, This[N, E]] = checkedSubtractEdge(
    e,
    simple = true,
    (outerEdge: E[N], innerEdge: EdgeT) => copy(nodes.toOuter, edges.toOuter.toBuffer -= outerEdge))

  final override protected def -!#(e: E[N]): This[N, E] = -!#?(e) getOrElse this

  protected def -!#?(e: E[N]): Either[ConstraintViolation, This[N, E]] = checkedSubtractEdge(
    e,
    simple = false,
    (outerEdge: E[N], innerEdge: EdgeT) =>
      copy(nodes.toOuter.toBuffer --= innerEdge.privateNodes map (n => n.value), edges.toOuter.toBuffer -= e))
}
