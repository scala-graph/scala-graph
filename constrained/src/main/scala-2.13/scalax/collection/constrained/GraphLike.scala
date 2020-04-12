package scalax.collection.constrained

import scalax.collection.GraphPredef.{
  EdgeLikeIn, InParam, InnerEdgeParam, InnerNodeParam, OutParam, OuterEdge, OuterNode, Param
}
import scalax.collection.config._
import scalax.collection.constrained.config.GenConstrainedConfig
import scalax.collection.constrained.generic.GraphConstrainedCompanion
import scalax.collection.{GraphLike => SimpleGraphLike}

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.Set
import scala.language.postfixOps

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
                E[+X] <: EdgeLikeIn[X],
                +This[X, Y[+X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Set[Param[X, Y]] with Graph[X, Y]]
    extends SimpleGraphLike[N, E, This]
    with GraphOps[N, E, This]
    with Constrained[N, E, This[N, E]] {
  this: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Set[Param[N, E]] with Graph[N, E] =>

  override val graphCompanion: GraphConstrainedCompanion[This]
  protected type Config <: GraphConfig with GenConstrainedConfig

  val constraintFactory: ConstraintCompanion[Constraint]
  override def stringPrefix: String = constraintFactory.stringPrefix getOrElse super.stringPrefix

  override protected def plusPlus(newNodes: Iterable[N], newEdges: Iterable[E[N]]): This[N, E] =
    graphCompanion.fromWithoutCheck[N, E](nodes.toOuter ++ newNodes, edges.toOuter ++ newEdges)(config)

  override protected def minusMinus(delNodes: Iterable[N], delEdges: Iterable[E[N]]): This[N, E] = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    graphCompanion.fromWithoutCheck[N, E](delNodesEdges._1, delNodesEdges._2)(config)
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

  def ++?(elems: IterableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]] = {
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

  def --?(elems: IterableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]] = {
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
                            nodes: => Iterable[N],
                            edges: => Iterable[E[N]]): Either[ConstraintViolation, This[N, E]] =
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
