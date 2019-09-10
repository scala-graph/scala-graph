package scalax.collection.constrained
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.{higherKinds, postfixOps}
import scala.collection.Set
import scala.collection.generic.{Growable, Shrinkable}
import scala.collection.mutable.Cloneable
import scala.reflect.ClassTag

import scalax.collection.GraphTraversalImpl
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.mutable.{ArraySet, BuilderImpl}
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.constrained.{Graph => CGraph, GraphLike => CGraphLike}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion
import config.GenConstrainedConfig

class GraphBuilder[N, E[X] <: EdgeLikeIn[X], GC[N, E[X] <: EdgeLikeIn[X]] <: CGraph[N, E] with CGraphLike[N, E, GC]](
    companion: GraphConstrainedCompanion[GC])(implicit edgeT: ClassTag[E[N]], config: GenConstrainedConfig)
    extends BuilderImpl[N, E, GC] {
  def result: This =
    companion.from(nodes, edges)(edgeT, config.asInstanceOf[companion.Config])
}

trait GraphLike[N, E[X] <: EdgeLikeIn[X], +This[X, Y[X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Graph[X, Y]]
    extends scalax.collection.mutable.GraphLike[N, E, This]
    with scalax.collection.constrained.GraphLike[N, E, This]
    with GraphOps[N, E, This]
    with Growable[Param[N, E]]
    with Shrinkable[Param[N, E]]
    with Cloneable[This[N, E]]
    with Mutable {
  selfGraph: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Graph[N, E] =>

  trait NodeSet extends super.NodeSet {

    protected def checkedRemove(fake: NodeT, ripple: Boolean): Either[ConstraintViolation, Boolean] =
      nodes find fake map { node =>
        def remove = withoutChecks {
          subtract(node, ripple, minus, minusEdges)
        }
        if (checkSuspended) Right(remove)
        else {
          val preCheckResult = preSubtract(node.asInstanceOf[self.NodeT], ripple)
          preCheckResult.followUp match {
            case Complete => Right(remove)
            case PostCheck =>
              val incidentEdges = node.edges.toBuffer
              if (remove) {
                postSubtract(selfGraph, Set(node), Set.empty[E[N]], preCheckResult).fold(
                  failure => {
                    withoutChecks {
                      selfGraph += node.value
                      selfGraph ++= incidentEdges
                    }
                    Left(failure)
                  },
                  _ => Right(true)
                )
              } else Right(false)
            case Abort => Left(preCheckResult)
          }
        }
      } getOrElse Right(false)

    final override def remove(node: NodeT): Boolean       = remove_?(node) getOrElse false
    final override def removeGently(node: NodeT): Boolean = removeGently_?(node) getOrElse false

    def remove_?(fake: NodeT): Either[ConstraintViolation, Boolean]       = checkedRemove(fake, ripple = true)
    def removeGently_?(fake: NodeT): Either[ConstraintViolation, Boolean] = checkedRemove(fake, ripple = false)
  }

  override def +(node: N): This[N, E] = +?(node) getOrElse this

  def +?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = clone += node,
      nodes = Set(node),
      edges = Set.empty)

  final override protected def +#(e: E[N]): This[N, E] = +#?(e) getOrElse this

  final protected def +#?(e: E[N]): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = edges contains Edge(e),
      preAdd = preAdd(e),
      copy = clone +=# e,
      nodes = Set.empty,
      edges = Set(e))

  override def ++=(elems: TraversableOnce[Param[N, E]]): this.type = ++=?(elems) getOrElse this

  def ++=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type] = {
    def add: this.type = withoutChecks { super.++=(elems) }
    if (checkSuspended) Right(add)
    else {
      def process(elems: Traversable[Param[N, E]]): Option[ConstraintViolation] = {
        val (filteredElems, newNodes, newEdges) = {
          val p     = new Param.Partitions[N, E]((elems filterNot contains).toSet)
          val edges = p.toOuterEdges
          (
            p.toInParams.toSeq,
            nodesToAdd(p.toOuterNodes, edges),
            edges
          )
        }
        val preCheckResult = preAdd(filteredElems: _*)
        if (preCheckResult.abort) Some(preCheckResult)
        else {
          add
          if (preCheckResult.postCheck) {
            postAdd(this, newNodes, newEdges, preCheckResult).fold(
              failure => {
                withoutChecks {
                  newNodes foreach super.remove
                  newEdges foreach super.remove
                }
                Some(failure)
              },
              _ => None
            )
          } else None
        }
      }
      (elems match {
        case elems: Traversable[Param[N, E]] => process(elems)
        case traversableOnce                 => process(traversableOnce.toSet)
      }).fold[Either[ConstraintViolation, this.type]](Right(this))(Left(_))
    }
  }

  final def -?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedMinusNode(node, forced = true, (outer: N, inner: NodeT) => { val c = clone; c -= outer; c })

  final override protected def -#(e: E[N]): This[N, E] = +#?(e) getOrElse this

  final protected def -#?(e: E[N]): Either[ConstraintViolation, This[N, E]] =
    checkedMinusEdge(e, simple = true, (outer: E[N], inner: EdgeT) => { val c = clone; c -=# outer; c })

  def --=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type] = {
    val (outerNodes, outerEdges) = {
      val p = partition(elems)
      (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    }
    val (innerNodes, innerEdges) = (outerNodes map find flatten, outerEdges map find flatten)
    val preCheckResult =
      preSubtract(innerNodes.asInstanceOf[Set[self.NodeT]], innerEdges.asInstanceOf[Set[self.EdgeT]], true)
    preCheckResult.followUp match {
      case Complete => Right(withoutChecks { super.--=(elems) })
      case PostCheck =>
        val subtractables = (elems filter this.contains).toArray ++ innerNodes.flatMap(_.edges).toBuffer
        withoutChecks { super.--=(subtractables) }
        postSubtract(this, outerNodes, outerEdges, preCheckResult).fold(
          failure => {
            withoutChecks { super.++=(subtractables) }
            Left(failure)
          },
          _ => Right(this)
        )
      case Abort => Left(preCheckResult)
    }
  }
}

import scalax.collection.constrained.generic.MutableGraphCompanion

trait Graph[N, E[X] <: EdgeLikeIn[X]]
    extends scalax.collection.mutable.Graph[N, E]
    with scalax.collection.constrained.Graph[N, E]
    with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E](edgeT, config)
}

object Graph extends MutableGraphCompanion[Graph] {
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
    DefaultGraphImpl.fromWithoutCheck[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)

  // TODO: canBuildFrom
}

abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]](iniNodes: Traversable[N] = Set[N](),
                                                          iniEdges: Traversable[E[N]] = Set[E[N]]())(
    implicit override val edgeT: ClassTag[E[N]],
    _config: DefaultGraphImpl.Config with GenConstrainedConfig with AdjacencyListArrayConfig)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {

  final override val graphCompanion = DefaultGraphImpl

  protected type Config = DefaultGraphImpl.Config
  final override def config = _config.asInstanceOf[graphCompanion.Config with Config]

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient protected[this] var _nodes: NodeSetT  = newNodeSet
  @inline final override def nodes                 = _nodes

  @transient protected[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges                = _edges

  initialize(iniNodes, iniEdges)

  @inline final override def empty = DefaultGraphImpl.empty(edgeT, config)

  @inline final override def clone: this.type =
    graphCompanion.fromWithoutCheck[N, E](nodes.toOuter, edges.toOuter)(edgeT, config).asInstanceOf[this.type]

  @SerialVersionUID(8082L)
  protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}

object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl] {
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    fromWithoutCheck(Set.empty, Set.empty)(edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(edgeT, config)

  final override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N, E] = from_?(nodes, edges) getOrElse empty[N, E](edgeT, config)

  def from_?[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Either[ConstraintViolation, DefaultGraphImpl[N, E]] = {
    def emptyGraph = empty[N, E](edgeT, config)

    if (nodes.isEmpty && edges.isEmpty) Right(empty[N, E](edgeT, config))
    else {
      val constraint     = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      val preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) Left(preCheckResult)
      else {
        val newGraph = fromWithoutCheck[N, E](nodes, edges)(edgeT, config)
        preCheckResult.followUp match {
          case Complete  => Right(newGraph)
          case PostCheck => constraint.postAdd(newGraph, nodes, edges, preCheckResult)
          case Abort     => Left(preCheckResult)
        }
      }
    }
  }
}

@SerialVersionUID(7701L)
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]](
    iniNodes: Traversable[N] = Nil,
    iniEdges: Traversable[E[N]] = Nil)(implicit override val edgeT: ClassTag[E[N]], _config: DefaultGraphImpl.Config)
    extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(edgeT, _config)
    with UserConstrainedGraph[N, E, DefaultGraphImpl[N, E]] {

  final override val self              = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint        = constraintFactory(this)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}
