package scalax.collection

import language.{higherKinds, implicitConversions}
import scala.annotation.unchecked.{uncheckedVariance => uV}

import scala.collection.{AbstractIterable, SeqFacade}
import GraphEdge.{EdgeLike, EdgeCopy, AbstractDiHyperEdge, AbstractDiEdge}
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
  type EdgeLikeIn[+N] = EdgeLike[N] with EdgeCopy[EdgeLike] with OuterEdge[N,EdgeLike] 
  /**
   * Denotes all directed edge types for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of directed edges
   * such as directed hyper-edges and directed edges.
   */
  type DiHyperEdgeLikeIn[+N] = AbstractDiHyperEdge[N] with EdgeCopy[AbstractDiHyperEdge] with OuterEdge[N,AbstractDiHyperEdge]
  /**
   * Denotes all directed edge types for the `E` type parameter of a `Graph`.
   * Supplying this type as the actual type parameter allows to include any kind of directed edges
   * such as directed hyper-edges and directed edges.
   */
  type DiEdgeLikeIn[+N] = AbstractDiEdge[N] with EdgeCopy[AbstractDiEdge] with OuterEdge[N,AbstractDiEdge]
  
  /** This algebraic type includes outer and inner nodes and edges. As such it serves as the
   *  type parameter to `SetLike` extending `Graph`. 
   */  
  sealed trait Param [+N, +E[X<:N @uV] <: EdgeLike[X]] {
    def isDefined = true
    
    def isNode: Boolean
    @inline final def isEdge: Boolean = ! isNode
    
    def isIn:   Boolean
    @inline final def isOut: Boolean = ! isIn
  }
  object Param {
    /**
     * Enables to query partitions of a collection of `Param`.
     */
    final class Partitions[N, E[X]<:EdgeLikeIn[X]](val elems: Traversable[Param[N,E]]) {
      lazy val partitioned = elems match {
        case g: Graph[N,E] => (g.nodes, g.edges)
        case x             => x partition (_.isNode)
      }
      def nodeParams = partitioned._1.asInstanceOf[Traversable[NodeParam[N]]]
      def edgeParams = partitioned._2.asInstanceOf[Traversable[EdgeParam]]
  
      def toOuterNodes: Traversable[N]    = nodeParams map (_.value)
      def toOuterEdges: Traversable[E[N]] = edgeParams map {_ match {
        case e: OuterEdge[N,E] => e.edge
        case e: InnerEdgeParam[N,E,_,E] => e.asEdgeTProjection[N,E].toOuter
      }}

      def toInParams: Traversable[InParam[N,E]] = elems map {_ match {
        case in: InParam[N,E] => in
        case n: InnerNodeParam[N]         => OuterNode(n.value)
        case e: InnerEdgeParam[N,E,_,E] => e.asEdgeTProjection[N,E].toOuter.asInstanceOf[OuterEdge[N,E]]
      }}
    }
  }
  implicit def graphParamsToPartition[N, E[X]<:EdgeLikeIn[X]]
              (elems: Traversable[Param[N,E]]) = new Param.Partitions[N,E](elems)
              
  implicit def nodeSetToOuter[N, E[X]<:EdgeLikeIn[X]](nodes: Graph[N,E]#NodeSetT): Iterable[N] =
    new AbstractIterable[N] {
      def iterator = new AbstractIterator[N] {
        private[this] val it = nodes.iterator
        def hasNext = it.hasNext
        def next = it.next.value
      }
    }
  
  implicit def nodeSetToSeq[N, E[X]<:EdgeLikeIn[X]](nodes: Graph[N,E]#NodeSetT)
      : Seq[OutParam[N,E]] = new SeqFacade(nodes)
  
  implicit def edgeSetToOuter[N, E[X]<:EdgeLikeIn[X]](edges: Graph[N,E]#EdgeSetT): Iterable[E[N]] =
    new AbstractIterable[E[N]] {
      def iterator = new AbstractIterator[E[N]] {
        private[this] val it = edges.iterator
        def hasNext = it.hasNext
        def next = it.next.toOuter
      }
    }
  
  implicit def edgeSetToSeq[N, E[X]<:EdgeLikeIn[X]](edges: Graph[N,E]#EdgeSetT)
      : Seq[OutParam[N,E]] = new SeqFacade(edges)

  /** @tparam N  the type of the nodes (vertices) this graph is passed to by the user.
   *  @tparam E  the kind of the edges (links) this graph is passed to by the user.
   */
  sealed trait InParam[+N, +E[X<:N @uV] <: EdgeLike[X]] extends Param[N,E] {
    def isIn  = true
  }
  /** Same as `InParam`. */
  type OuterElem[N, +E[X<:N] <: EdgeLike[X]] = InParam[N,E]

  sealed trait OutParam[+NO, +EO[X<:NO @uV] <: EdgeLike[X]] extends Param[NO,EO] {
    def isIn  = false
  }
  trait NodeParam[+N] {
    def value: N
    def isNode = true 
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
  trait InnerNodeParam[+NI] extends OutParam[NI,Nothing] with NodeParam[NI] {
    def isContaining[N, E[X]<:EdgeLikeIn[X]](g: GraphBase[N,E]): Boolean

    protected[collection] final
    def asNodeT[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G): g.NodeT = this.asInstanceOf[g.NodeT]

    protected[collection] final
    def asNodeTProjection[N <: NI @uV, E[X]<:EdgeLikeIn[X]]: GraphBase[N,E]#NodeT =
      this.asInstanceOf[Graph[N,E]#NodeT]

    final def fold[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton, T]
        (g: G)(fa: g.NodeT => T, fb: GraphBase[N,E]#NodeT => T): T =
      if (isContaining[N,E](g)) fa(asNodeT[N,E,G](g))
      else                      fb(asNodeTProjection[N,E])

    final def toNodeT[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G)(f: GraphBase[N,E]#NodeT => g.NodeT): g.NodeT =
      fold[N,E,G,g.NodeT](g)(n => n, f)
  }
  object InnerNodeParam {
    def unapply[NI] (nodeOut: InnerNodeParam[NI]): Option[NI] = Some(nodeOut.value)
  }
  
  sealed trait EdgeParam {
    def isNode = false 
  }
  
  /** Classes implementing `EdgeLike` must be instantiated mixing in this trait.
   *  This is a precondition for passing edge-instances to a `Graph`.
   *
   * @tparam NI  the type of the nodes (vertices) this graph is passed to by the user.
   * @tparam EI  the kind of the edges (links) this graph is passed to by the user.
   */
  trait OuterEdge[+NI, +EI[X<:NI @uV] <: EdgeLike[X]]
    extends InParam[NI,EI] with EdgeParam
  { this: EI[NI @uV] =>
    def edge: EI[NI @uV] = this
  }
  
  /** @tparam NI  the type of the nodes the graph is passed to.
   *  @tparam EI  the kind of the edges the graph is passed to.
   *  @tparam NO  the type of the nodes created internally.
   *  @tparam EO  the kind of the edges created internally.
   */
  trait InnerEdgeParam[+NI, +EI[X<:NI @uV] <: EdgeLike[X], +NO <: InnerNodeParam[NI], +EO[X<:NO @uV] <: EdgeLike[X]]
    extends OutParam[NI,EI] with EdgeParam
  { 
    def edge: EO[NO @uV]
    final def isContaining[N <: NI @uV, E[X]<:EdgeLikeIn[X]](g: GraphBase[N,E]): Boolean =
      edge._n(0).isContaining[N,E](g)

    protected[collection] final
    def asEdgeT[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G) : g.EdgeT = this.asInstanceOf[g.EdgeT]

    protected[collection] final
    def asEdgeTProjection[N <: NI @uV, E[X]<:EdgeLikeIn[X]]: GraphBase[N,E]#EdgeT =
      this.asInstanceOf[Graph[N,E]#EdgeT]

    final def fold[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton, T]
        (g: G)(fa: g.EdgeT => T, fb: GraphBase[N,E]#EdgeT => T): T =
      if (isContaining[N,E](g)) fa(asEdgeT[N,E,G](g))
      else                      fb(asEdgeTProjection[N,E])

    final def toEdgeT[N <: NI @uV, E[X]<:EdgeLikeIn[X], G <: GraphBase[N,E] with Singleton]
        (g: G)(f: GraphBase[N,E]#EdgeT => g.EdgeT): g.EdgeT =
      fold[N,E,G,g.EdgeT](g)(e => e, f)
      
    def stringPrefix = "" 
    override def toString = if (stringPrefix.length > 0) stringPrefix + "(" + edge + ")"
                            else edge.toString
  }
  object InnerEdgeParam {
    @inline implicit def toEdge[NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EO[X<:NO] <: EdgeLike[X]]
      (innerEdge: InnerEdgeParam[NI,EI,NO,EO]): EO[NO] = innerEdge.edge
  }
  //-----------------------------------------------------------------------//
  import GraphEdge._
  
  @inline implicit def anyToNode[N](n: N) = OuterNode(n)
  implicit def seqToGraphParam[N, E[X<:N] <: EdgeLikeIn[X]](s: Seq[N]): Seq[InParam[N,E]] = s map {_ match {
    case e: EdgeLike[_] with EdgeCopy[_] with OuterEdge[_,_] with InParam[_,_] => e.asInstanceOf[InParam[N,E]]
    case e: InnerEdgeParam[_,_,_,_] => e.edge.asInstanceOf[OuterEdge[N,E]]
    case e: EdgeLike[_] => throw new IllegalArgumentException("Invalid edge type: EdgeCopy and OuterEdge need be mixed in.")
    case InnerNodeParam(n) => OuterNode(n).asInstanceOf[InParam[N,E]]
    case n => OuterNode(n)
  }}
  def nodePredicate [NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EO[X<:NO] <: EdgeLike[X]]
      (pred: NI => Boolean): Param[NI,EI] => Boolean = param =>
    if (param.isIn) false
    else if(param.isNode) pred(param.asInstanceOf[InnerNodeParam[NI]].value)
    else param.asInstanceOf[InnerEdgeParam[NI,EI,NO,EO]].edge forall (n => pred(n.value))

  @inline implicit def predicateToNodePredicate
      [NI, EI[X<:NI] <: EdgeLike[X], NO <: InnerNodeParam[NI], EC[X<:NO] <: EdgeLike[X]]
      (p: NI => Boolean) =
    nodePredicate[NI,EI,NO,EC](p)

  final implicit class EdgeAssoc[N1](val n1: N1) extends AnyVal {
    @inline def ~ [N >: N1](n2: N) = new UnDiEdge[N](n1, n2) 
    @inline def ~>[N >: N1](n2: N) = new   DiEdge[N](n1, n2)
  }

  final implicit class HyperEdgeAssoc[NOld](val e: EdgeLikeIn[NOld]) extends AnyVal {
    def ~ [N >: NOld](n: N)(implicit endpointsKind: CollectionKind = Bag): HyperEdge[N] = {
      require(e.isUndirected)
      HyperEdge.from[N](NodeProduct(e.iterator.toBuffer += n))
    }
    def ~>[N >: NOld](n: N)(implicit targetsKind: CollectionKind = Bag): DiHyperEdge[N] = {
      require(e.isDirected)
      DiHyperEdge.from[N](NodeProduct(e.iterator.toBuffer += n))
    }
  }
}