package scalax.collection.constrained
package mutable

import scala.language.{higherKinds, postfixOps}
import scala.collection.{Set, Iterable}
import scala.collection.generic.{CanBuildFrom, Growable, Shrinkable}
import scala.collection.mutable.{Builder, Cloneable, ListBuffer, Set => MutableSet}

import scalax.collection.{Graph => CommonGraph, GraphTraversalImpl}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                                      GraphParamNode, NodeIn, NodeOut, EdgeIn, EdgeOut}
import scalax.collection.mutable.{ArraySet, BuilderImpl}
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.io._

import scalax.collection.constrained.{Graph => CGraph, GraphLike => CGraphLike}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion
import config.GenConstrainedConfig

class GraphBuilder[N,
                   E[X] <: EdgeLikeIn[X],
                   GC[N,E[X] <: EdgeLikeIn[X]] <: CGraph[N,E] with CGraphLike[N,E,GC]]
      (companion: GraphConstrainedCompanion[GC])
      (implicit edgeManifest: Manifest[E[N]],
       config: GenConstrainedConfig)
  extends BuilderImpl[N,E,GC]
{
  def result: This =
    companion.from(nodes, edges)(edgeManifest, config.asInstanceOf[companion.Config])
}
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
               +This[X, Y[X]<:EdgeLikeIn[X]] <: GraphLike[X,Y,This] with Graph[X,Y]]
	extends scalax.collection.mutable.GraphLike[N, E, This]
	with	  scalax.collection.constrained.GraphLike[N, E, This]
  with    Growable  [GraphParam[N,E]]
	with	  Shrinkable[GraphParam[N,E]] 
	with	  Cloneable [Graph[N,E]] 
  with    Mutable
{ selfGraph: This[N,E] =>
  trait NodeSet extends super.NodeSet {
    /** generic constrained subtraction */
    protected def checkedRemove(node: NodeT, ripple: Boolean): Boolean = {
      def remove = withoutChecks { subtract(node, ripple,  minus, minusEdges) }
      var removed, handle = false
      if (checkSuspended) removed = remove
      else {
        val preCheckResult = preSubtract(node.asInstanceOf[self.NodeT], ripple)
        preCheckResult.followUp match { 
          case Complete  => removed = remove
          case PostCheck => removed = remove
            if (removed &&
                ! postSubtract(selfGraph, Set(node), Set.empty[E[N]], preCheckResult)) {
              handle = true
              selfGraph  += node.value
              selfGraph ++= node.edges
            }
          case Abort     => handle = true
        }
      }
      if (handle) onSubtractionRefused(Set(node.asInstanceOf[self.NodeT]),
                                       Set.empty[self.EdgeT], selfGraph)
      removed && ! handle
    }
    override def remove      (node: NodeT) = checkedRemove(node, true)
    override def removeGently(node: NodeT) = checkedRemove(node, false)
  }
  /** generic checked addition */
  protected def checkedAdd[G >: This[N,E]]
            ( contained: => Boolean,
              preAdd:    => PreCheckResult,
              copy:      => G,
              nodes:     => Iterable[N],
              edges:     => Iterable[E[N]] ): This[N,E] =
  { if (contained) this
    else if (checkSuspended) copy.asInstanceOf[This[N,E]]
    else {
      var graph = this
      var handle = false
      val preCheckResult = preAdd
      preCheckResult.followUp match { 
        case Complete  => graph = copy.asInstanceOf[This[N,E]]
        case PostCheck => graph = copy.asInstanceOf[This[N,E]]
          if (! postAdd(graph, nodes, edges, preCheckResult)) {
            handle = true
            graph = this
          }
        case Abort     => handle = true
      }
      if (handle) onAdditionRefused(nodes, edges, this)
      graph
    }
  }
  override def + (node: N) = 
    checkedAdd (contained = nodes contains Node(node),
                preAdd    = preAdd(node),
                copy      = clone += node,
                nodes     = Set(node),
                edges     = Set.empty[E[N]])

  override def ++=(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { elems match {
      case elems: Iterable[GraphParam[N,E]] => 
        val p = new GraphParam.Partitions[N,E](elems)
        val inFiltered = p.toInParams.toSet.filter(elem => ! (this contains elem)).toSeq 
        var handle = false
        val preCheckResult = preAdd(inFiltered: _*)
        if (preCheckResult.abort)
          handle = true
        else {
          withoutChecks { super.++=(elems) }
          if (preCheckResult.postCheck) {
            val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
            if (! postAdd(this, outerNodes, outerEdges, preCheckResult)) {
              handle = true
              withoutChecks {
                super.--=(allNodes(outerNodes, outerEdges) map (n => NodeIn(n)))
              }
            }
          }
        }
        if (handle) onAdditionRefused(p.toOuterNodes, p.toOuterEdges, this)

      case _ => throw new IllegalArgumentException("Iterable expected")
    }
    this
  } 
  override def --=(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { lazy val p = partition(elems)
    lazy val (outerNodes, outerEdges) = (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    def innerNodes =
       (outerNodes.view map (this find _) filter (_.isDefined) map (_.get) force).toSet
    def innerEdges =
       (outerEdges.view map (this find _) filter (_.isDefined) map (_.get) force).toSet

    type C_NodeT = self.NodeT
    type C_EdgeT = self.EdgeT
    var handle = false
    val preCheckResult = preSubtract(innerNodes.asInstanceOf[Set[C_NodeT]],
                                     innerEdges.asInstanceOf[Set[C_EdgeT]], true)
    preCheckResult.followUp match { 
      case Complete  => withoutChecks { super.--=(elems) }
      case PostCheck =>
        val subtractables = elems filter (this contains _)
        withoutChecks { super.--=(subtractables) }
        if (! postSubtract(this, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          withoutChecks { super.++=(subtractables) }
        }
      case Abort     => handle = true
    }
    if (handle) onSubtractionRefused(innerNodes.asInstanceOf[Set[C_NodeT]],
                                     innerEdges.asInstanceOf[Set[C_EdgeT]], this)
    this
  }
}

import scalax.collection.constrained.generic.{MutableGraphCompanion}

trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.mutable.Graph[N,E]
  with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph]
{
  override def empty: Graph[N,E] = Graph.empty[N,E](edgeManifest, config)
}
object Graph
  extends MutableGraphCompanion[Graph]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                               config: Config): Graph[N,E] =
    DefaultGraphImpl.empty[N,E](edgeManifest, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): DefaultGraphImpl[N,E] =
    DefaultGraphImpl.fromUnchecked[N,E](nodes, edges)(edgeManifest, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)(edgeManifest, config)

  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Graph[N,E] =
    DefaultGraphImpl.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)(
                                     edgeManifest, config)
  // TODO: canBuildFrom
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set[N](),
    iniEdges: Iterable[E[N]] = Set[E[N]]())
   (implicit override val edgeManifest: Manifest[E[N]],
    _config: DefaultGraphImpl.Config with GenConstrainedConfig with AdjacencyListArrayConfig)
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl]
  with    GraphTraversalImpl[N,E]
{
  @transient override final val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config
  override final def config = _config.asInstanceOf[graphCompanion.Config with Config]

  class NodeSet extends super[AdjacencyListGraph].NodeSet with super[Graph].NodeSet
  @inline final def newNodeSet: NodeSetT = new NodeSet
  override final val nodes = newNodeSet 
  override final val edges = new EdgeSet 
  initialize(iniNodes, iniEdges)

  @inline final override def empty = DefaultGraphImpl.empty(edgeManifest, config)
  @inline final override def clone(): this.type = {
    graphCompanion.from[N,E](nodes.toNodeInSet, edges.toEdgeInSet)(
                             edgeManifest, config).asInstanceOf[this.type]
  }
  @SerialVersionUID(8082L)
  protected class NodeBase(value: N, hints: ArraySet.Hints)
    extends InnerNodeImpl(value, hints)
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}
object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                               config: Config) =
    from(Set.empty[N], Set.empty[E[N]])(edgeManifest, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                              edges: Iterable[E[N]])
                                             (implicit edgeManifest: Manifest[E[N]],
                                              config: Config): DefaultGraphImpl[N,E] =
    new UserConstrainedGraphImpl[N,E](nodes, edges)(edgeManifest, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config) : DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty 
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = fromUnchecked[N,E](nodes, edges)(edgeManifest, config)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      var handle = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = ! constraint.postAdd(newGraph, nodes, edges, preCheckResult)
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
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      iniNodes:    Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      iniEdges:    Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config) : DefaultGraphImpl[N,E] =
  {
    var preCheckResult = PreCheckResult(Abort)
    if (iniNodes.nonEmpty || iniEdges.nonEmpty) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      preCheckResult = constraint.preCreate(iniNodes, iniEdges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (iniNodes, iniEdges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E]()(edgeManifest, config) {
      from(nodeStreams, iniNodes, edgeStreams, iniEdges)
    }
    var handle = false
    preCheckResult.followUp match {
      case Complete  =>
      case PostCheck => handle = ! newGraph.postAdd(newGraph, iniNodes, iniEdges, preCheckResult)
      case Abort     => handle = true
    }
    if (handle) {
      newGraph.onAdditionRefused(iniNodes, iniEdges, newGraph)
      empty[N,E](edgeManifest, config)
    } else
      newGraph
  }
  // TODO canBuildFrom
}
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set.empty[N],
    iniEdges: Iterable[E[N]] = Set.empty[E[N]])
   (implicit override val edgeManifest: Manifest[E[N]],
    _config: DefaultGraphImpl.Config)
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)(edgeManifest, _config)
  with    UserConstrainedGraph[N,E]
{
  final override val self = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint  = constraintFactory(this)
}
