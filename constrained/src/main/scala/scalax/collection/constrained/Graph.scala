package scalax.collection.constrained

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, postfixOps}
import scala.collection.{GenTraversableOnce, Set}
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.{
  EdgeLikeIn, InParam, InnerEdgeParam, InnerNodeParam, OutParam, OuterEdge, OuterNode, Param
}
import scalax.collection.{Graph => SimpleGraph, GraphLike => SimpleGraphLike}
import scalax.collection.config.GraphConfig
import generic.GraphConstrainedCompanion
import config._

/** A template trait for graphs.
  *
  * This trait provides the common structure and operations of immutable graphs independently
  * of its representation.
  *
  * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
  *
  * @tparam N    the user type of the nodes (vertices) in this graph.
  * @tparam E    the higher kinded type of the edges (links) in this graph.
  * @tparam This the higher kinded type of the graph itself.
  * @author Peter Empen
  */
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
                +This[X, Y[X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Set[Param[X, Y]] with Graph[X, Y]]
    extends SimpleGraphLike[N, E, This]
    with GraphOps[N, E, This]
    with Constrained[N, E, This[N, E]] {
  this: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Set[Param[N, E]] with Graph[N, E] =>

  override val graphCompanion: GraphConstrainedCompanion[This]
  protected type Config <: GraphConfig with GenConstrainedConfig

  val constraintFactory: ConstraintCompanion[Constraint]
  override def stringPrefix: String = constraintFactory.stringPrefix getOrElse super.stringPrefix

  override protected def plusPlus(newNodes: Traversable[N], newEdges: Traversable[E[N]]): This[N, E] =
    graphCompanion.fromWithoutCheck[N, E](nodes.toOuter ++ newNodes, edges.toOuter ++ newEdges)(edgeT, config)

  override protected def minusMinus(delNodes: Traversable[N], delEdges: Traversable[E[N]]): This[N, E] = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    graphCompanion.fromWithoutCheck[N, E](delNodesEdges._1, delNodesEdges._2)(edgeT, config)
  }

  @transient private var suspended      = false
  protected def checkSuspended: Boolean = suspended
  final protected def withoutChecks[R](exec: => R): R = {
    val old = suspended
    suspended = true
    val res = exec
    suspended = old
    res
  }

  import PreCheckFollowUp._

  def +?(elem: Param[N, E]): Either[ConstraintViolation, This[N, E]] = elem match {
    case in: InParam[N, E] =>
      in match {
        case n: OuterNode[N]    => this +? n.value
        case e: OuterEdge[N, E] => this +#? e.edge
      }
    case out: OutParam[_, _] =>
      out match {
        case n: InnerNodeParam[N]          => this +? n.value
        case e: InnerEdgeParam[N, E, _, E] => this +#? e.asEdgeT[N, E, this.type](this).toOuter
      }
  }

  protected def +#?(e: E[N]): Either[ConstraintViolation, This[N, E]]

  def ++?(elems: GenTraversableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]] = {
    val (outerNodes, outerEdges, preCheckResult) = {
      val it = elems match {
        case x: Iterable[Param[N, E]]        => x
        case x: TraversableOnce[Param[N, E]] => x.toIterable
        case _                               => throw new IllegalArgumentException("TraversableOnce expected.")
      }
      val p = new Param.Partitions[N, E](it filter (elm => !(this contains elm)))
      (p.toOuterNodes, p.toOuterEdges, preAdd(p.toInParams.toSet.toSeq: _*))
    }
    preCheckResult.followUp match {
      case Complete  => Right(plusPlus(outerNodes, outerEdges))
      case PostCheck => postAdd(plusPlus(outerNodes, outerEdges), outerNodes, outerEdges, preCheckResult)
      case Abort     => Left(preCheckResult)
    }
  }

  def -?(elem: Param[N, E]): Either[ConstraintViolation, This[N, E]] = elem match {
    case in: InParam[N, E] =>
      in match {
        case n: OuterNode[N]    => this -? n.value
        case e: OuterEdge[N, E] => this -#? e.edge
      }
    case out: OutParam[_, _] =>
      out match {
        case n: InnerNodeParam[N]          => this -? n.value
        case e: InnerEdgeParam[N, E, _, E] => this -#? e.asEdgeT[N, E, this.type](this).toOuter
      }
  }

  protected def -#?(e: E[N]): Either[ConstraintViolation, This[N, E]]

  def --?(elems: GenTraversableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]] = {
    lazy val p                        = partition(elems)
    lazy val (outerNodes, outerEdges) = (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    def innerNodes =
      (outerNodes.view map (this find _) filter (_.isDefined) map (_.get) force).toSet
    def innerEdges =
      (outerEdges.view map (this find _) filter (_.isDefined) map (_.get) force).toSet

    type C_NodeT = self.NodeT
    type C_EdgeT = self.EdgeT
    val preCheckResult = preSubtract(innerNodes.asInstanceOf[Set[C_NodeT]], innerEdges.asInstanceOf[Set[C_EdgeT]], true)
    preCheckResult.followUp match {
      case Complete => Right(minusMinus(outerNodes, outerEdges))
      case PostCheck =>
        postSubtract(minusMinus(outerNodes, outerEdges), outerNodes, outerEdges, preCheckResult)
      case Abort => Left(preCheckResult)
    }
  }

  protected def checkedPlus(contained: => Boolean,
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
        case Abort     => Left(preCheckResult)
      }
    }

  protected def checkedMinusNode(node: N,
                                 forced: Boolean,
                                 copy: (N, NodeT) => This[N, E] @uV): Either[ConstraintViolation, This[N, E]] =
    nodes find node map { innerNode =>
      def subtract = copy(node, innerNode)
      if (checkSuspended)
        Right(subtract)
      else {
        val preCheckResult = preSubtract(innerNode.asInstanceOf[self.NodeT], forced)
        preCheckResult.followUp match {
          case Complete  => Right(subtract)
          case PostCheck => postSubtract(subtract, Set(node), Set.empty[E[N]], preCheckResult)
          case Abort     => Left(preCheckResult)
        }
      }
    } getOrElse Right(this)

  protected def checkedMinusEdge(edge: E[N],
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
          case Abort     => Left(preCheckResult)
        }
      }
    } getOrElse Right(this)
}

// ----------------------------------------------------------------------------
/** A trait for dynamically constrained graphs.
  *
  * @tparam N    the type of the nodes (vertices) in this graph.
  * @tparam E    the kind of the edges in this graph.
  * @author Peter Empen
  */
trait Graph[N, E[X] <: EdgeLikeIn[X]] extends Set[Param[N, E]] with SimpleGraph[N, E] with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

/** Default factory for constrained graphs.
  * Graph instances returned from this factory will be immutable.
  *
  * @author Peter Empen
  */
object Graph extends GraphConstrainedCompanion[Graph] {
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config) =
    immutable.Graph.newBuilder[N, E](edgeT, config)

  def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): Graph[N, E] =
    immutable.Graph.empty[N, E](edgeT, config)
  def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig): Graph[N, E] =
    immutable.Graph.from[N, E](nodes, edges)(edgeT, config)
  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): Graph[N, E] =
    immutable.Graph.fromWithoutCheck[N, E](nodes, edges)(edgeT, config)
}

trait UserConstrainedGraph[N, E[X] <: EdgeLikeIn[X], +G <: Graph[N, E]] { _: Graph[N, E] with Constrained[N, E, G] =>
  val constraint: Constraint[N, E, G] @uV

  private type C_NodeT = constraint.self.NodeT
  private type C_EdgeT = constraint.self.EdgeT

  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]) =
    constraint preCreate (nodes, edges)
  override def preAdd(node: N)               = constraint preAdd node
  override def preAdd(edge: E[N])            = constraint preAdd edge
  override def preAdd(elems: InParam[N, E]*) = constraint preAdd (elems: _*)
  override def postAdd(newGraph: G @uV,
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult) =
    constraint postAdd (newGraph, passedNodes, passedEdges, preCheck)

  override def preSubtract(node: self.NodeT, forced: Boolean) =
    constraint preSubtract (node.asInstanceOf[C_NodeT], forced)
  override def preSubtract(edge: self.EdgeT, simple: Boolean) =
    constraint preSubtract (edge.asInstanceOf[C_EdgeT], simple)

  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean) =
    constraint preSubtract (nodes.asInstanceOf[Set[C_NodeT]],
    edges.asInstanceOf[Set[C_EdgeT]],
    simple)

  override def postSubtract(newGraph: G @uV,
                            passedNodes: Traversable[N],
                            passedEdges: Traversable[E[N]],
                            preCheck: PreCheckResult) =
    constraint postSubtract (newGraph, passedNodes, passedEdges, preCheck)
}
