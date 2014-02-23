package scalax.collection

import language.{higherKinds, implicitConversions}

import scala.collection.{Abstract, SeqFacade}
import GraphEdge.{EdgeLike, EdgeCopy, DiHyperEdgeLike}
/**
 * This object serves as a container for several `Graph`-related definitions like
 * parameter-types and implicit conversions.
 * 
 * You will usually simply import all its members along with the members of `InnerEdgeParam`:
 * {{{
 * import scalax.collection.GraphPredef._, scalax.collection.InnerEdgeParam._
 * }}}
 * @author Peter Empen
 */
object GraphPredef {
  /**
   * The most generic type for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of edges
   * such as hyper-edges, undirected and directed edges.
   */
  type EdgeLikeIn[N] = EdgeLike[N] with EdgeCopy[EdgeLike] with OuterEdge[N,EdgeLike] 
  /**
   * Denotes all directed edge types for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of directed edges
   * such as directed hyper-edges and directed edges.
   */
  type DiHyperEdgeLikeIn[N] = DiHyperEdgeLike[N] with EdgeCopy[DiHyperEdgeLike] with OuterEdge[N,DiHyperEdgeLike]
  
  /** This algebraic type includes outer and inner nodes and edges. As such it serves as the
   *  type parameter to `SetLike` extending `Graph`. 
   */  
  sealed trait Param [N, +E[X<:N] <: EdgeLike[X]] {
    def isDefined = true
    def isNode: Boolean
    def isEdge: Boolean
    def isIn:   Boolean
    def isOut:  Boolean
  }
  object Param {
    /**
     * Enables to query partitions of a collection of `Param`.
     */
    final class Partitions[N, E[X]<:EdgeLikeIn[X]](val elems: Iterable[Param[N,E]]) {
      lazy val partitioned = elems match {
        case g: Graph[N,E] => (g.nodes, g.edges)
        case x             => x partition (_.isNode)
      }
      def nodeParams = partitioned._1.asInstanceOf[Iterable[NodeParam[N]]]
      def edgeParams = partitioned._2.asInstanceOf[Iterable[EdgeParam]]
  
      def toOuterNodes: Iterable[N]    = nodeParams map (_.value)
      def toOuterEdges: Iterable[E[N]] = edgeParams map {_ match {
        case e: OuterEdge[N,E] => e.edge
        case e: InnerEdgeParam[N,E,_,E] => e.asEdgeTProjection[N,E].toEdgeIn
      }}

      def toInParams: Iterable[InParam[N,E]] = elems map {_ match {
        case in: InParam[N,E] => in
        case n: InnerNodeParam[N]         => OuterNode(n.value)
        case e: InnerEdgeParam[N,E,_,E] => e.asEdgeTProjection[N,E].toEdgeIn.asInstanceOf[OuterEdge[N,E]]
      }}
    }
  }
  implicit def graphParamsToPartition[N, E[X]<:EdgeLikeIn[X]]
              (elems: Iterable[Param[N,E]]) = new Param.Partitions[N,E](elems)
              
  implicit def nodeSetToOuter[N, E[X]<:EdgeLikeIn[X]](nodes: Graph[N,E]#NodeSetT)
      : Iterable[N] =
    new Abstract.Iterable[N] {
      def iterator = new Abstract.Iterator[N] {
        private[this] val it = nodes.iterator
        def hasNext = it.hasNext
        def next = it.next.value
      }
    }
  
  implicit def nodeSetToSeq[N, E[X]<:EdgeLikeIn[X]](nodes: Graph[N,E]#NodeSetT)
      : Seq[OutParam[N,E]] = new SeqFacade(nodes)
  
  implicit def edgeSetToOuter[N, E[X]<:EdgeLikeIn[X]](edges: Graph[N,E]#EdgeSetT)
      : Iterable[E[N]] =
    new Abstract.Iterable[E[N]] {
      def iterator = new Abstract.Iterator[E[N]] {
        private[this] val it = edges.iterator
        def hasNext = it.hasNext
        def next = it.next.toEdgeIn
      }
    }
  
  implicit def edgeSetToSeq[N, E[X]<:EdgeLikeIn[X]](edges: Graph[N,E]#EdgeSetT)
      : Seq[OutParam[N,E]] = new SeqFacade(edges)

  /** @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   *  @tparam EI  the kind of the edges (links) this graph is passed to by the user.
   */
  sealed trait InParam [NI, +EI[X<:NI] <: EdgeLike[X]] extends Param[NI,EI] {
    def isIn  = true
    def isOut = false
  }
  sealed trait OutParam[NO, +EO[X<:NO] <: EdgeLike[X]] extends Param[NO,EO] {
    def isIn  = false
    def isOut = true
  }
  trait NodeParam[N] {
    def value: N
    def isNode = true 
    def isEdge = false
    def stringPrefix = "" 
    override def toString = if (stringPrefix.length > 0) stringPrefix + "(" + value + ")"
                            else value.toString
  }
  
  /** @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   */
  case class OuterNode[NI] (override val value: NI) 
    extends InParam[NI, Nothing]
       with NodeParam[NI]

  /** @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   */
  trait InnerNodeParam[NI] extends OutParam[NI,Nothing] with NodeParam[NI] {
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
  object InnerNodeParam {
    def unapply[NI] (nodeOut: InnerNodeParam[NI]): Option[NI] = Some(nodeOut.value)
  }
  
  /** @tparam N  the type of the nodes.
   *  @tparam E  the kind of the edges.
   */
  sealed trait EdgeParam
  {
    def isNode = false 
    def isEdge = true
  }
  
  /** Classes implementing `EdgeLike` must be instantiated mixing in this trait.
   *  This is a precondition for passing edge-instances to a `Graph`.
   *
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   * @tparam EI  the kind of the edges (links) this graph is passed to by the user.
   */
  trait OuterEdge  [NI, +EI[X<:NI] <: EdgeLike[X]]
    extends InParam[NI,EI] with EdgeParam
  { this: EI[NI] =>
    def edge: EI[NI] = this
  }
  
  /** @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   *  @tparam NO  the type of the nodes (vertices) this graph passes back.
   *  @tparam EC  the kind of the edges (links) contained in edges of type EdgeT this graph passes back.
   */
  trait InnerEdgeParam [NI, +EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], +EO[X<:NO] <: EdgeLike[X]]
    extends OutParam[NI,EI] with EdgeParam
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
  object InnerEdgeParam {
    @inline implicit def toEdge[NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EO[X<:NO] <: EdgeLike[X]]
      (innerEdge: InnerEdgeParam[NI,EI,NO,EO]) = innerEdge.edge
  }
  //-----------------------------------------------------------------------//
  import GraphEdge._
  
  @inline implicit def anyToNode[N]     (n: N) = OuterNode(n)
  @inline implicit def seqToGraphParam[N, E[X<:N] <: EdgeLikeIn[X]](s: Seq[N]): Seq[InParam[N,E]] =
    s map {_ match {case e: EdgeLike[N] with EdgeCopy[EdgeLike] with OuterEdge[N,E] => e
                    case e: EdgeLike[N] => throw new IllegalArgumentException(
                        "Before being passed to a graph, edge-types need to mix in EdgeCopy and OuterEdge.")
// TODO               case e: InnerEdgeParam [N,InnerNodeParam[N,E,EdgeCont[N]],E,EdgeCont[N]] =>
//                            e.edge match {case e: DiEdge[N] => newDiEdgeIn[N](e.nodes)}
                    case n => OuterNode(n)
//                    case InnerNodeParam(n) => OuterNode(n)
          }}
  def nodePredicate [NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EO[X<:NO] <: EdgeLike[X]]
      (pred: NI => Boolean) =
    (out: Param[NI,EI]) => out match {
      case n: InnerNodeParam[NI] => pred(n.value)
      case e: InnerEdgeParam[NI,EI,NO,EO] => e.edge forall (n => pred(n.value))
      case _ => false
    }

//  def edgePredicate [NI, NO <: InnerNodeParam[NI], EC[X<:NO] <: EdgeLike[X]] (pred: EC[NO] => Boolean) =
//    (out: Param[N,E]) => out match {
//      case n: InnerNodeParam[NI] => false
//      case e: InnerEdgeParam[NI,NO,EC] => pred(e.edge) 
//      case _ => false
//    }

  @inline implicit def predicateToNodePredicate
      [NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EC[X<:NO] <: EdgeLike[X]]
      (p: NI => Boolean) =
    nodePredicate[NI,EI,NO,EC](p)

//  @inline implicit def predicateToEdgePredicate[NI, NO <: InnerNodeParam[NI], EC[X<:NO] <: EdgeLike[X]]
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