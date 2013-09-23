package scalax.collection

import language.{higherKinds, implicitConversions}

import GraphEdge.{EdgeLike, EdgeCopy, DiHyperEdgeLike}
/**
 * This object serves as a container for several `Graph`-related definitions like
 * parameter-types and implicit conversions.
 * 
 * You will usually simply import all its members along with the members of `EdgeOut`:
 * {{{
 * import scalax.collection.GraphPredef._, scalax.collection.EdgeOut._
 * }}}
 * @author Peter Empen
 */
object GraphPredef {
  /**
   * The most generic type for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of edges
   * such as hyper-edges, undirected and directed edges.
   */
  type EdgeLikeIn[N] = EdgeLike[N] with EdgeCopy[EdgeLike] with EdgeIn[N,EdgeLike] 
  /**
   * Denotes all directed edge types for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of directed edges
   * such as directed hyper-edges and directed edges.
   */
  type DiHyperEdgeLikeIn[N] = DiHyperEdgeLike[N] with EdgeCopy[DiHyperEdgeLike] with EdgeIn[N,DiHyperEdgeLike]
  /**
   * This algebraic type serves as the type parameter to SetLike.
   * 
   * @author Peter Empen
   */  
  sealed trait GraphParam [N, +E[X<:N] <: EdgeLike[X]]
  {
    def isNode: Boolean
    def isEdge: Boolean
    def isIn:   Boolean
    def isOut:  Boolean
  }
  object GraphParam {
    /**
     * Enables to query partitions of a collection of `GraphParam`.
     */
    final class Partitions[N, E[X]<:EdgeLikeIn[X]](val elems: Iterable[GraphParam[N,E]])
    {
      lazy val partitioned = elems match {
        case g: Graph[N,E] => (g.nodes, g.edges)
        case x             => x partition (_.isNode)
      }
      def nodeParams = partitioned._1.asInstanceOf[Iterable[GraphParamNode[N]]]
      def edgeParams = partitioned._2.asInstanceOf[Iterable[GraphParamEdge]]
  
      def toOuterNodes: Iterable[N]    = nodeParams map (_.value)
      def toOuterEdges: Iterable[E[N]] = edgeParams map {_ match {
        case e: EdgeIn[N,E] => e.edge
        case e: EdgeOut[N,E,_,E] => e.asEdgeTProjection[N,E].toEdgeIn
      }}

      def toInParams: Iterable[GraphParamIn[N,E]] = elems map {_ match {
        case in: GraphParamIn[N,E] => in
        case n: NodeOut[N]         => NodeIn(n.value)
        case e: EdgeOut[N,E,_,E] => e.asEdgeTProjection[N,E].toEdgeIn.asInstanceOf[EdgeIn[N,E]]
      }}
    }
  }
  implicit def graphParamsToPartition[N, E[X]<:EdgeLikeIn[X]]
              (elems: Iterable[GraphParam[N,E]]) = new GraphParam.Partitions[N,E](elems)
  /**
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   * @tparam EI  the kind of the edges (links) this graph is passed to by the user.
   */
  sealed trait GraphParamIn [NI, +EI[X<:NI] <: EdgeLike[X]] extends GraphParam[NI,EI]
  {
    def isIn  = true
    def isOut = false
  }
  sealed trait GraphParamOut[NO, +EO[X<:NO] <: EdgeLike[X]] extends GraphParam[NO,EO]
  {
    def isIn  = false
    def isOut = true
  }
  trait GraphParamNode[N]
  {
    def value: N
    def isNode = true 
    def isEdge = false
    def stringPrefix = "" 
    override def toString = if (stringPrefix.length > 0) stringPrefix + "(" + value + ")"
                            else value.toString
  }
  /**
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   */
  case class NodeIn[NI] (override val value: NI) 
    extends GraphParamIn[NI, Nothing]
    with    GraphParamNode[NI]
  /**
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   */
  trait NodeOut[NI] extends GraphParamOut[NI,Nothing] with GraphParamNode[NI] {
    def isContaining[N, E[X]<:EdgeLikeIn[X]](g: GraphBase[N,E]): Boolean

    protected[collection] final
    def asNodeT[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G): g.NodeT = this.asInstanceOf[g.NodeT]

    protected[collection] final
    def asNodeTProjection[N <: NI, E[X]<:EdgeLikeIn[X]]: GraphBase[N,E]#NodeT =
      this.asInstanceOf[Graph[N,E]#NodeT]

    final def fold[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton, T]
        (g: G)(fa: g.NodeT => T, fb: GraphBase[N,E]#NodeT => T): T =
      if (isContaining[N,E](g)) fa(asNodeT[N,E,G](g))
      else                      fb(asNodeTProjection[N,E])

    final def toNodeT[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G)(f: GraphBase[N,E]#NodeT => g.NodeT): g.NodeT =
      fold[N,E,G,g.NodeT](g)(n => n, f)
  }
  object NodeOut {
    def unapply[NI] (nodeOut: NodeOut[NI]): Option[NI] = Some(nodeOut.value)
  }
  /**
   * @tparam N  the type of the nodes.
   * @tparam E  the kind of the edges.
   */
  sealed trait GraphParamEdge
  {
    def isNode = false 
    def isEdge = true
  }
  /**
   * Classes implementing `EdgeLike` must be instantiated mixing in this trait.
   * This is a precondition for passing edge-instances to a `Graph`.
   *
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   * @tparam EI  the kind of the edges (links) this graph is passed to by the user.
   */
  trait EdgeIn  [NI, +EI[X<:NI] <: EdgeLike[X]]
    extends GraphParamIn[NI,EI] with GraphParamEdge
  { this: EI[NI] =>
    def edge: EI[NI] = this
  }
  /**
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   * @tparam NO  the type of the nodes (vertices) this graph passes back.
   * @tparam EC  the kind of the edges (links) contained in edges of type EdgeT this graph passes back.
   */
  trait EdgeOut [NI, +EI[X<:NI] <: EdgeLike[X], NO <: NodeOut[NI], +EO[X<:NO] <: EdgeLike[X]]
    extends GraphParamOut[NI,EI] with GraphParamEdge
  { 
    def edge: EO[NO]
    final def isContaining[N <: NI, E[X]<:EdgeLikeIn[X]](g: GraphBase[N,E]): Boolean =
      edge._1.isContaining[N,E](g)

    protected[collection] final
    def asEdgeT[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G) : g.EdgeT = this.asInstanceOf[g.EdgeT]

    protected[collection] final
    def asEdgeTProjection[N <: NI, E[X]<:EdgeLikeIn[X]]: GraphBase[N,E]#EdgeT =
      this.asInstanceOf[Graph[N,E]#EdgeT]

    final def fold[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton, T]
        (g: G)(fa: g.EdgeT => T, fb: GraphBase[N,E]#EdgeT => T): T =
      if (isContaining[N,E](g)) fa(asEdgeT[N,E,G](g))
      else                      fb(asEdgeTProjection[N,E])

    final def toEdgeT[N <: NI, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G)(f: GraphBase[N,E]#EdgeT => g.EdgeT): g.EdgeT =
      fold[N,E,G,g.EdgeT](g)(e => e, f)
      
    def stringPrefix = "" 
    override def toString = if (stringPrefix.length > 0) stringPrefix + "(" + edge + ")"
                            else edge.toString
  }
  object EdgeOut {
    @inline implicit def toEdge[NI, EI[X<:NI] <: EdgeLike[X], NO <: NodeOut[NI], EO[X<:NO] <: EdgeLike[X]]
      (innerEdge: EdgeOut[NI,EI,NO,EO]) = innerEdge.edge
  }
  //-----------------------------------------------------------------------//
  import GraphEdge._
  
  @inline implicit def anyToNode[N]     (n: N) = NodeIn(n)
  @inline implicit def seqToGraphParam[N, E[X<:N] <: EdgeLikeIn[X]](s: Seq[N]): Seq[GraphParamIn[N,E]] =
    s map {_ match {case e: EdgeLike[N] with EdgeCopy[EdgeLike] with EdgeIn[N,E] => e
                    case e: EdgeLike[N] => throw new IllegalArgumentException(
                        "Before being passed to a graph, edge-types need to mix in EdgeCopy and EdgeIn.")
// TODO               case e: EdgeOut [N,NodeOut[N,E,EdgeCont[N]],E,EdgeCont[N]] =>
//                            e.edge match {case e: DiEdge[N] => newDiEdgeIn[N](e.nodes)}
                    case n => NodeIn(n)
//                    case NodeOut(n) => NodeIn(n)
          }}
  def nodePredicate [NI, EI[X<:NI] <: EdgeLike[X], NO <: NodeOut[NI], EO[X<:NO] <: EdgeLike[X]]
      (pred: NI => Boolean) =
    (out: GraphParam[NI,EI]) => out match {
      case n: NodeOut[NI] => pred(n.value)
      case e: EdgeOut[NI,EI,NO,EO] => e.edge forall (n => pred(n.value))
      case _ => false
    }

//  def edgePredicate [NI, NO <: NodeOut[NI], EC[X<:NO] <: EdgeLike[X]] (pred: EC[NO] => Boolean) =
//    (out: GraphParam[N,E]) => out match {
//      case n: NodeOut[NI] => false
//      case e: EdgeOut[NI,NO,EC] => pred(e.edge) 
//      case _ => false
//    }

  @inline implicit def predicateToNodePredicate
      [NI, EI[X<:NI] <: EdgeLike[X], NO <: NodeOut[NI], EC[X<:NO] <: EdgeLike[X]]
      (p: NI => Boolean) =
    nodePredicate[NI,EI,NO,EC](p)

//  @inline implicit def predicateToEdgePredicate[NI, NO <: NodeOut[NI], EC[X<:NO] <: EdgeLike[X]]
//                                               (p: EC[NO] => Boolean) = edgePredicate[NI,NO,EC](p)

  final implicit class EdgeAssoc[N1](val n1: N1) extends AnyVal {
    @inline def ~ [N >: N1, N2 <: N](n2: N2) = new UnDiEdge[N](Tuple2(n1, n2)) 
    @inline def ~>[N >: N1, N2 <: N](n2: N2) = new   DiEdge[N](Tuple2(n1, n2))
  }

  final implicit class HyperEdgeAssoc[NOld](val e: EdgeLikeIn[NOld]) extends AnyVal {
    @inline def ~ [N >: NOld, NX <: N](n: NX) = { assume (e.undirected)
      new   HyperEdge[N](NodeProduct(e.iterator.toBuffer += n))
    }
    @inline def ~>[N >: NOld, NX <: N](n: NX) = { assume (e.directed)
      new DiHyperEdge[N](NodeProduct(e.iterator.toBuffer += n))
    }
  }
}