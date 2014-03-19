package scalax.collection.constrained

import scala.language.{higherKinds, postfixOps}
import scala.collection.{Set, SetLike, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe._

import scalax.collection.GraphPredef.{EdgeLikeIn, Param,
       InParam, OutParam, seqToGraphParam, OuterNode, InnerNodeParam, OuterEdge, InnerEdgeParam}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.{GraphLike => SimpleGraphLike, Graph => SimpleGraph}
import scalax.collection.config.GraphConfig

import generic.GraphConstrainedCompanion
import config._

/**
 * A template trait for graphs.
 * 
 * This trait provides the common structure and operations of immutable graphs independently
 * of its representation.
 * 
 * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the higher kinded type of the edges (links) in this graph.
 * @tparam This the higher kinded type of the graph itself.
 *
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X]  <: EdgeLikeIn[X],
                +This[X, Y[X]<:EdgeLikeIn[X]]
                      <: GraphLike[X,Y,This] with Set[Param[X,Y]] with Graph[X,Y]]
  extends SimpleGraphLike[N,E,This]
  with    Constrained[N,E]
{ this: This[N,E] =>
  override val graphCompanion: GraphConstrainedCompanion[This]
  protected type Config <: GraphConfig with GenConstrainedConfig

  val constraintFactory: ConstraintCompanion[Constraint]
  override def stringPrefix = constraintFactory.stringPrefix getOrElse super.stringPrefix

  override protected def plusPlus(newNodes: Iterable[N],
                                  newEdges: Iterable[E[N]]): This[N,E] =
    graphCompanion.
    fromUnchecked[N,E](nodes.toOuter ++ newNodes,
                       edges.toOuter ++ newEdges)(
                       edgeT,
                       config).asInstanceOf[This[N,E]]
  override protected def minusMinus(delNodes: Iterable[N],
                                    delEdges: Iterable[E[N]]): This[N,E] = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    graphCompanion.
      fromUnchecked[N,E](delNodesEdges._1, delNodesEdges._2)(
                         edgeT, config).asInstanceOf[This[N,E]]
  }
  /** This flag is used to prevent constraint checking for single additions and
   * subtractions triggered by a multiple addition/subtraction such as `++=`.
   */
  @transient protected var checkSuspended = false
  protected final def withoutChecks[R] (exec: => R): R = {
    val oldSuspended = checkSuspended
    checkSuspended = true
      val res = exec
    checkSuspended = oldSuspended
    res
  }

  import PreCheckFollowUp._
  override def ++ (elems: GenTraversableOnce[Param[N,E]]): this.type =
  { var graph = this
    val it = elems match {
      case x: Iterable       [Param[N,E]] => x
      case x: TraversableOnce[Param[N,E]] => x.toIterable
      case _ => throw new IllegalArgumentException("TraversableOnce expected.")
    }
    val p = new Param.Partitions[N,E](it filter (elm => !(this contains elm)))
    val inFiltered = p.toInParams.toSet.toSeq
    val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
    var handle = false
    val preCheckResult = preAdd(inFiltered: _*)
    preCheckResult.followUp match { 
      case Complete  => graph = plusPlus(outerNodes, outerEdges)
      case PostCheck => graph = plusPlus(outerNodes, outerEdges)
        if (! postAdd(graph, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          graph = this
        }
      case Abort     => handle = true
    }
    if (handle) onAdditionRefused(outerNodes, outerEdges, graph)

    graph.asInstanceOf[this.type]
  } 
  override def -- (elems: GenTraversableOnce[Param[N,E]]) =
  { var graph = this

    lazy val p = partition(elems)
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
      case Complete  => graph = minusMinus(outerNodes, outerEdges)
      case PostCheck => graph = minusMinus(outerNodes, outerEdges)
        if (! postSubtract(graph, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          graph = this
        }
      case Abort     => handle = true
    }
    if (handle) onSubtractionRefused(innerNodes, innerEdges, graph)

    graph.asInstanceOf[this.type]
  }
}

// ----------------------------------------------------------------------------
/**
 * A trait for dynamically constrained graphs.
 * 
 * @tparam N    the type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
  extends Set        [Param[N,E]]
  with    SimpleGraph[N,E]
  with    GraphLike  [N,E,Graph]
{
  override def empty: Graph[N,E] = Graph.empty[N,E]
}
/**
 * Default factory for constrained graphs.
 * Graph instances returned from this factory will be immutable.
 * 
 * @author Peter Empen
 */
object Graph
  extends GraphConstrainedCompanion[Graph]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]]
     (implicit edgeT: TypeTag[E[N]],
      config: Config) =
    immutable.Graph.newBuilder[N,E](edgeT, config)

  def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                      config: Config = defaultConfig): Graph[N,E] =
    immutable.Graph.empty[N,E](edgeT, config)
  def from[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                     edges: Iterable[E[N]])
                                    (implicit edgeT: TypeTag[E[N]],
                                     config: Config = defaultConfig): Graph[N,E] =
    immutable.Graph.from[N,E](nodes, edges)(edgeT, config)
  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]](nodes:    Iterable[N],
                                              edges:    Iterable[E[N]])
                                             (implicit edgeT: TypeTag[E[N]],
                                              config: Config = defaultConfig): Graph[N,E] =
    immutable.Graph.fromUnchecked[N,E](nodes, edges)(edgeT, config)
}
trait UserConstrainedGraph[N, E[X] <: EdgeLikeIn[X]]
  extends Graph[N,E]
{
  val constraint: Constraint[N,E]
 /*
   * delegating from Constrained to Constraint;
   * asInstanceOf is safe because 'this' is set to 'constraint.self' 
   */
  private type C_NodeT = constraint.self.NodeT
  private type C_EdgeT = constraint.self.EdgeT

  override def preCreate(nodes: collection.Iterable[N],
                         edges: collection.Iterable[E[N]]) =
                                    constraint preCreate (nodes, edges)
  override def preAdd(node: N   ) = constraint preAdd node
  override def preAdd(edge: E[N]) = constraint preAdd edge
  override def preAdd(elems: InParam[N,E]*) = constraint preAdd (elems: _*)
  override def postAdd (newGraph   : Graph[N,E],
                        passedNodes: Iterable[N],
                        passedEdges: Iterable[E[N]],
                        preCheck   : PreCheckResult) =
    constraint postAdd (newGraph, passedNodes, passedEdges, preCheck)

  override def preSubtract (node: self.NodeT, forced: Boolean) =
    constraint preSubtract (node.asInstanceOf[C_NodeT], forced)
  override def preSubtract (edge: self.EdgeT, simple: Boolean) =
    constraint preSubtract (edge.asInstanceOf[C_EdgeT], simple)
  override def preSubtract (nodes: => Set[self.NodeT],
                            edges: => Set[self.EdgeT], simple: Boolean) =
    constraint preSubtract (nodes.asInstanceOf[Set[C_NodeT]],
                            edges.asInstanceOf[Set[C_EdgeT]],
                            simple)
  override def postSubtract(newGraph   : Graph[N,E],
                            passedNodes: Iterable[N],
                            passedEdges: Iterable[E[N]],
                            preCheck   : PreCheckResult) =
    constraint postSubtract (newGraph, passedNodes, passedEdges, preCheck)

  override def onAdditionRefused   (refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) =
    constraint onAdditionRefused   (refusedNodes, refusedEdges, graph)
  override def onSubtractionRefused(refusedNodes: Iterable[Graph[N,E]#NodeT],
                                    refusedEdges: Iterable[Graph[N,E]#EdgeT],
                                    graph:        Graph[N,E]) =
    constraint onSubtractionRefused(refusedNodes.asInstanceOf[Iterable[C_NodeT]],
                                    refusedEdges.asInstanceOf[Iterable[C_EdgeT]],
                                    graph)
}
