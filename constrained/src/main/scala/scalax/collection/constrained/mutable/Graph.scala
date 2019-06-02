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
    with Growable[Param[N, E]]
    with Shrinkable[Param[N, E]]
    with Cloneable[This[N, E]]
    with Mutable {
  selfGraph: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Graph[N, E] =>

  trait NodeSet extends super.NodeSet {

    /** generic constrained subtraction */
    protected def checkedRemove(node: NodeT, ripple: Boolean): Boolean = {
      var removed, handle = false
      def remove          = withoutChecks { subtract(node, ripple, minus, minusEdges) }
      if (checkSuspended) removed = remove
      else {
        val preCheckResult = preSubtract(node.asInstanceOf[self.NodeT], ripple)
        preCheckResult.followUp match {
          case Complete => removed = remove
          case PostCheck =>
            val edges = node.edges.toBuffer
            removed = remove
            if (removed &&
                !postSubtract(selfGraph, Set(node), Set.empty[E[N]], preCheckResult)) {
              handle = true
              withoutChecks {
                selfGraph += node.value
                selfGraph ++= edges
              }
            }
          case Abort => handle = true
        }
      }
      if (handle) onSubtractionRefused(Set(node), Set.empty[EdgeT], selfGraph)
      removed && !handle
    }
    override def remove(node: NodeT)       = checkedRemove(node, true)
    override def removeGently(node: NodeT) = checkedRemove(node, false)
  }

  /** generic checked addition */
  protected def checkedAdd[G >: This[N, E]](contained: => Boolean,
                                            preAdd: => PreCheckResult,
                                            copy: => G,
                                            nodes: => Traversable[N],
                                            edges: => Traversable[E[N]]): This[N, E] =
    if (contained) this
    else if (checkSuspended) copy.asInstanceOf[This[N, E]]
    else {
      var graph          = this
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
      graph
    }

  override def +(node: N) =
    checkedAdd(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = clone += node,
      nodes = Set(node),
      edges = Set.empty[E[N]])

  override def ++=(elems: TraversableOnce[Param[N, E]]): this.type = {
    def add = withoutChecks { super.++=(elems) }
    if (checkSuspended) add
    else {
      def process(elems: Traversable[Param[N, E]]): Unit = {
        val (filteredElems, newNodes, newEdges) = {
          val p     = new Param.Partitions[N, E]((elems filterNot contains).toSet)
          val edges = p.toOuterEdges
          (
            p.toInParams.toSeq,
            nodesToAdd(p.toOuterNodes, edges),
            edges
          )
        }
        var handle         = false
        val preCheckResult = preAdd(filteredElems: _*)
        if (preCheckResult.abort)
          handle = true
        else {
          add
          if (preCheckResult.postCheck) {
            if (!postAdd(this, newNodes, newEdges, preCheckResult)) {
              handle = true
              withoutChecks {
                newNodes foreach super.remove
                newEdges foreach super.remove
              }
            }
          }
        }
        if (handle) onAdditionRefused(newNodes, newEdges, this)
      }
      elems match {
        case elems: Traversable[Param[N, E]] => process(elems)
        case traversableOnce                 => process(traversableOnce.toSet)
      }
    }
    this
  }

  override def --=(elems: TraversableOnce[Param[N, E]]): this.type = {
    val (outerNodes, outerEdges) = {
      val p = partition(elems)
      (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    }
    val (innerNodes, innerEdges) = (outerNodes map find flatten, outerEdges map find flatten)

    type C_NodeT = self.NodeT
    type C_EdgeT = self.EdgeT

    var handle         = false
    val preCheckResult = preSubtract(innerNodes.asInstanceOf[Set[C_NodeT]], innerEdges.asInstanceOf[Set[C_EdgeT]], true)
    preCheckResult.followUp match {
      case Complete => withoutChecks { super.--=(elems) }
      case PostCheck =>
        val subtractables = (elems filter this.contains).toArray ++ innerNodes.flatMap(_.edges).toBuffer
        withoutChecks { super.--=(subtractables) }
        if (!postSubtract(this, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          withoutChecks { super.++=(subtractables) }
        }
      case Abort => handle = true
    }
    if (handle) onSubtractionRefused(innerNodes, innerEdges, this)
    this
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
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
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

  class NodeSet extends super[AdjacencyListGraph].NodeSet with super[Graph].NodeSet

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
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config) =
    from(Set.empty[N], Set.empty[E[N]])(edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N, E] = {
    val existElems     = nodes.nonEmpty || edges.nonEmpty
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N, E](edgeT, config)
      val constraint = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) {
        constraint onAdditionRefused (nodes, edges, emptyGraph)
        return emptyGraph
      }
    }
    val newGraph = fromWithoutCheck[N, E](nodes, edges)(edgeT, config)
    if (existElems) {
      val emptyGraph = empty[N, E](edgeT, config)
      val constraint = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      var handle     = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = !constraint.postAdd(newGraph, nodes, edges, preCheckResult)
        case Abort     => handle = true
      }
      if (handle) {
        constraint.onAdditionRefused(nodes, edges, newGraph)
        emptyGraph
      } else
        newGraph
    } else
      newGraph
  }
  // TODO canBuildFrom
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
