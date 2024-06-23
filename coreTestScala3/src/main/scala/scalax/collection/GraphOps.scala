package scalax.collection

import scala.reflect.ClassTag

import scalax.collection.generic._

/** Operations common to mutable and immutable graphs.
  *
  * @define mapGeneric You can call this flavor only if this graph's edge type is generic.
  * @define mapTyped   You can call this flavor only if this graph's edge type is typed.
  *
  * @define mapNodes Creates a new graph with nodes mapped by `fNode` and with an untouched edge structure otherwise.
  * @define mapEdges Creates a new graph with nodes and edges that are computed by the supplied mapping functions.
  *
  * @define flatMapNodes Creates a new graph with nodes returned by `fNode` and an edge structure that remains intact where possible.
  * @define flatMapEdges Creates a new graph with nodes and edges returned by `fNode` respectively `fEdge`.
  *
  * @define seeOverload See overload except the parameter
  *
  * @define fNode To apply to all nodes of this graph.
  *               Since the inner node is passed you can also examine the node context.
  *               Call `outer` to get the value of type `N` of the node.
  * @define fNodeFlat If `fNode` returns several new nodes with none equaling to the original node,
  *                   the first new node is accepted to be the result of the node transformation.
  *                   For more flexibility pass your own edge mapper to the overload.
  * @define fEdge To apply to all edges of this graph.
  *               This function is passed the current inner edge and its ends after being mapped by `fNode`.
  *               Since the inner edge is passed you can also examine its context.
  *               Call `outer` to get the outer edge of type E.
  * @define simplifiedFEdge has a simplified signature in this overload leaving out the inner edge.
  *                         This comes in handy whenever you don't need to inspect inner edges.
  * @define fEdgeOption To apply to any directed or undirected edge in this possibly mixed graph.
  *                     If not present simple edges will be mapped by the mandatory edge mapper you supply.
  *                     You are recommended supplying `Some` unless you know that the graph does not contain any simple edge.
  * @define fHyperEdge To apply to all hyperedges in this graph.
  *                    This function is passed the current inner hyperedge and its ends after being mapped by `fNode`.
  *                    Since the inner hyperedge is passed you can also examine its context.
  *                    Call `outer` to get the outer hyperedge of type E.
  * @define fDiHyperEdge To apply to all directed hyperedges in this graph.
  *                      This function is passed the existing inner directed hyperedge and its sources and targets after being mapped by `fNode`.
  *                      Since the inner directed hyperedge is passed you can also examine the edge context.
  *                      Call `outer` to get the outer directed hyperedge of type E.
  * @define fDiHyperEdgeOption To apply to any directed hyperedge in this possibly mixed graph.
  *                            If not present directed hyperedges will be mapped by the mandatory `fDiHyperEdge`.
  *                            You are recommended supplying `Some` unless you know that the graph does not contain any directed hyperedge.
  *
  * @define mapNN The node type of the resulting graph which may be unchanged or different from this graph's node type.
  * @define mapEC The higher kind of the generic edge type parameter of this graph.
  * @define mapECTyped The edge type parameter of this graph.
  *
  * @define mapReturn The mapped graph with possibly changed node and edge type parameters.
  * @define flatMapReturn A new graph of possibly changed node and edge types and of any new structure depending on your edge mapper(s).
  */
trait GraphOps[N, E <: Edge[N], +CC[X, Y <: Edge[X]]] extends OuterElems[N, E] {

  /** Whether this graph contains any node or any edge. */
  @inline final def isEmpty: Boolean = iterator.isEmpty

  /** Whether all edges of this graph are directed. */
  def isDirected: Boolean

  /** Whether this graph contains at least one hyperedge. */
  def isHyper: Boolean

  /** Whether this graph contains at least one directed and one undirected edge. */
  def isMixed: Boolean

  /** Whether this graph contains at least one multi-edge. We defnie multi-edges by
    *    a. two or more directed edges having the same source and target
    *    a. two or more undirected edges connecting the same nodes
    *    a. two or more (directed) hyperedges that, after being decomposed into (directed) edges,
    * yield any multy-edge as stipulated above.
    */
  def isMulti: Boolean

  /** `true` if this graph has at most 1 node. */
  @inline final def isTrivial: Boolean = order <= 1

  /** `true` if this graph has at least 2 nodes. */
  @inline final def nonTrivial: Boolean = !isTrivial

  /** The order, commonly referred to as |G|, of this graph equaling to the number of nodes. */
  def order: Int

  /** The size, commonly referred to as ||G||, of this graph equaling to the number of edges. */
  def size: Int

  /** The number of nodes and edges. */
  @inline final def elementCount: Int = order + size

  /** The Sum of the weight of all edges. */
  def totalWeight: Double

  sealed trait InnerElem

  type NodeT <: InnerNode
  trait InnerNode extends InnerElem {

    /** The outer node as supplied by instantiation or addition. */
    def outer: N

    protected[collection] def asNodeT: NodeT
  }
  object InnerNode {
    def unapply(node: InnerNode): Some[(NodeT, N)] = Some(node.asNodeT, node.outer)
  }

  type EdgeT <: InnerEdge
  trait InnerEdge extends InnerElem {

    /** The outer edge as supplied by instantiation or addition. */
    def outer: E

    protected[collection] def asEdgeT: EdgeT
  }
  object InnerEdge {
    def unapply(edge: InnerEdge): Some[(EdgeT, E)] = Some(edge.asEdgeT, edge.outer)
  }

  /** Iterator over all inner nodes and edges. */
  def iterator: Iterator[InnerElem]

  /** Iterable over all nodes and edges. */
  def toIterable: Iterable[InnerElem]

  /** Iterator over all inner nodes and edges. */
  def outerIterator: Iterator[OuterElem]

  /** Iterable over all nodes and edges. */
  def toOuterIterable: Iterable[OuterElem]

  /** Creates a new graph by adding all `edges` and `isolatedNodes` omitting duplicates.
    * The new graph is upcasted if any of the arguments is an upcast of `N` respectively `E`.
    * Use `union` to concatenate all nodes and edges of another graph.
    *
    * @param isolatedNodes to be concatenated. Nodes that are implicitly defined by any edge in `edges` will be ignored.
    * @param edges to be concatenated.
    */
  def concat[N2 >: N, E2 >: E <: Edge[N2]](isolatedNodes: IterableOnce[N2], edges: IterableOnce[E2])(implicit
      e: E2 <:< Edge[N2]
  ): CC[N2, E2]

  /** Same as `concat(isolatedNodes, edges)` but with empty `isolatedNodes`.
    * This method is useful if you don't need to pass any isolated node.
    */
  def concat[N2 >: N, E2 >: E <: Edge[N2]](edges: IterableOnce[E2])(implicit e: E2 <:< Edge[N2]): CC[N2, E2] =
    concat[N2, E2](Nil, edges)(e)

  /** Alias for `concat(isolatedNodes, edges)`. */
  @inline final def ++[N2 >: N, E2 >: E <: Edge[N2]](isolatedNodes: IterableOnce[N2], edges: IterableOnce[E2])(implicit
      e: E2 <:< Edge[N2]
  ): CC[N2, E2] =
    concat(isolatedNodes, edges)(e)

  /** Alias for `concat(edges)`. */
  @inline final def ++[N2 >: N, E2 >: E <: Edge[N2]](edges: IterableOnce[E2])(implicit
      e: E2 <:< Edge[N2]
  ): CC[N2, E2] =
    concat[N2, E2](edges)(e)

  /** Computes the union between this graph and `that` graph. */
  @inline final def union[N2 >: N, E2 >: E <: Edge[N2]](that: AnyGraph[N2, E2]): CC[N2, E2] =
    concat(that.nodes.outerIterator, that.edges.outerIterator)

  /** Whether the given outer node is contained in this graph. */
  def contains(node: N): Boolean

  /** Whether the given outer edge is contained in this graph. */
  def contains(edge: E): Boolean

  type NodePredicate = NodeT => Boolean
  type EdgePredicate = EdgeT => Boolean

  /** Node predicate with constant `true`. */
  def anyNode: NodePredicate

  /** Node predicate with constant `false`. */
  def noNode: NodePredicate

  /** Edge predicate with constant `true`. */
  def anyEdge: EdgePredicate

  /** Edge predicate with constant `false`. */
  def noEdge: EdgePredicate

  /** Whether the given node is contained in this graph. */
  // TODO make implementation allocation-free
  @inline final def apply(node: N): Boolean = find(node).isDefined

  /** Whether the given edge is contained in this graph. */
  @inline final def apply(edge: E): Boolean = find(edge).isDefined

  /** Computes a new graph with nodes satisfying `nodeP` and edges satisfying `edgeP`.
    * If both `nodeP` and `edgeP` have default values the original graph is retained.
    */
  def filter(nodeP: NodePredicate = anyNode, edgeP: EdgePredicate = anyEdge): CC[N, E]

  /** Computes a new graph without nodes satisfying `nodeP` and without edges satisfying `ePred`.
    * If both `nodeP` and `ePred` have default values the original graph is retained.
    */
  def filterNot(nodeP: NodePredicate = noNode, edgeP: EdgePredicate = noEdge): CC[N, E] =
    filter(n => !nodeP(n), e => !edgeP(e))

  /** Searches this graph for an inner node that wraps an outer node equalling to the given outer node. */
  def find(node: N): Option[NodeT]

  /** Searches this graph for an inner edge that wraps an outer edge equalling to the given outer edge. */
  def find(edge: E): Option[EdgeT]

  /** Short for `find(node).get`.
    *
    * @throws NoSuchElementException if the node is not found.
    */
  def get(node: N): NodeT

  /** Short for `find(edge).get`.
    *
    * @throws NoSuchElementException if the edge is not found.
    */
  def get(edge: E): EdgeT

  protected def removedAll(isolatedNodes: IterableOnce[N], edges: IterableOnce[E]): CC[N, E]

  /** Computes a new graph that is the difference of this graph and `that` graph. */
  final def diff(that: AnyGraph[N, E]): CC[N, E] = removedAll(that.nodes.outerIterator, that.edges.outerIterator)

  /** Alias for `diff`. */
  @inline final def &~(that: AnyGraph[N, E]): CC[N, E] = this diff that

  /** Computes the intersection between this graph and `that` graph. */
  final def intersect(that: AnyGraph[N, E]): CC[N, E] = this filter (n => that(n.outer), e => that(e.outer))

  /** Alias for `intersect`. */
  @inline final def &(that: AnyGraph[N, E]): CC[N, E] = this intersect that

  /** Alias for `union`. */
  @inline final def |(that: AnyGraph[N, E]): CC[N, E] = this union that

  /** $mapNodes
    *
    * $mapGeneric Otherwise see `mapBound`.
    *
    * If this graph also contains typed edges, the typed edge's partial `map` function will be called to replace the ends.
    * If the partial function is not defined, there will be an attempt to fall back to a generic edge.
    * If that attempt also fails the edge will be dropped.
    * So, if you have a mixed graph with generic and typed edges, prefer mapping edges directly to avoid leaving edges out.
    *
    * @tparam NN   $mapNN
    * @tparam EC   $mapEC
    * @param fNode $fNode
    * @return      $mapReturn
    */
  /* @param w1    Ensures that the type parameter `E` of this graph is of type `GenericMapper`.
   * @param w2    Captures the higher kind of current `E` to determine the return type.
   */
  def map[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, t: ClassTag[EC[NN]]): CC[NN, EC[NN]]

  /** $mapNodes
    *
    * $mapTyped Otherwise see `map`.
    *
    * @param fNode $fNode
    * @return      $mapReturn
    */
  /* @param w1 Ensures that the type parameter `E` of this graph is of type `PartialMapper`.
   */
  def mapBound(fNode: NodeT => N)(implicit w1: E <:< PartialMapper): CC[N, E]

  /** $mapEdges
    *
    * $mapGeneric Otherwise see `mapBound`.
    *
    * @tparam NN   $mapNN
    * @tparam EC   $mapEC
    * @param fNode $fNode
    * @param fEdge $fEdge
    * @return      $mapReturn
    */
  /* @param w witnesses that this graph is defined as a non-hypergraph by its `E` type parameter.
   */
  def map[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fEdge: (EdgeT, NN, NN) => EC[NN]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC[NN]]

  /** $mapEdges
    *
    * $seeOverload
    *
    * @param fEdge $simplifiedFEdge
    */
  final def map[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fEdge: (NN, NN) => EC[NN]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC[NN]] =
    map(fNode, (_, n1: NN, n2: NN) => fEdge(n1, n2))

  /** $mapEdges
    *
    * $mapTyped Otherwise see `map`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode $fNode
    * @param fEdge $fEdge
    * @return $mapReturn
    */
  /* @param w witnesses that this graph is defined as a non-hypergraph by its `E` type parameter.
   */
  def mapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fEdge: (EdgeT, NN, NN) => EC
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC]

  /** $mapEdges
    *
    * $seeOverload
    *
    * @param fEdge $simplifiedFEdge
    */
  final def mapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fEdge: (NN, NN) => EC
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC] =
    mapBound(fNode, (_, n1: NN, n2: NN) => fEdge(n1, n2))

  /** $mapEdges
    *
    * $mapGeneric Otherwise see `mapHyperBound`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode $fNode
    * @param fHyperEdge $fHyperEdge
    * @param fDiHyperEdge $fDiHyperEdgeOption
    * @param fEdge $fEdgeOption
    * @return $mapReturn
    */
  /* @param w witnesses that this graph is defined as a hypergraph by its `E` type parameter.
   */
  def mapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fHyperEdge: (EdgeT, Several[NN]) => EC[NN],
      fDiHyperEdge: Option[(EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC[NN]],
      fEdge: Option[(EdgeT, NN, NN) => EC[NN]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]]

  /** $mapEdges
    *
    * $mapGeneric Otherwise see `mapHyperBound`.
    *
    * $seeOverload
    *
    * @param fHyperEdge $simplifiedFEdge
    */
  final def mapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fHyperEdge: Several[NN] => EC[NN],
      fDiHyperEdge: Option[(OneOrMore[NN], OneOrMore[NN]) => EC[NN]] = None,
      fEdge: Option[(NN, NN) => EC[NN]] = None
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]] =
    mapHyper(
      fNode,
      (_, several: Several[NN]) => fHyperEdge(several),
      fDiHyperEdge.map(f => (_, sources: OneOrMore[NN], targets: OneOrMore[NN]) => f(sources, targets)),
      fEdge.map(f => (_, n1: NN, n2: NN) => f(n1, n2))
    )

  /** $mapEdges
    *
    * $mapTyped Otherwise see `mapHyper`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode $fNode
    * @param fHyperEdge $fHyperEdge
    * @param fDiHyperEdge $fDiHyperEdgeOption
    * @param fEdge $fEdgeOption
    * @return $mapReturn
    */
  /* @param w witnesses that this graph is defined as a hypergraph by its `E` type parameter.
   */
  def mapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fHyperEdge: (EdgeT, Several[NN]) => EC,
      fDiHyperEdge: Option[(EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC],
      fEdge: Option[(EdgeT, NN, NN) => EC]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC]

  /** $mapEdges
    *
    * $mapTyped Otherwise see `mapHyper`.
    *
    * $seeOverload
    *
    * @param fHyperEdge $simplifiedFEdge
    */
  final def mapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fHyperEdge: Several[NN] => EC,
      fDiHyperEdge: Option[(OneOrMore[NN], OneOrMore[NN]) => EC] = None,
      fEdge: Option[(NN, NN) => EC] = None
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC] =
    mapHyperBound(
      fNode,
      (_, several: Several[NN]) => fHyperEdge(several),
      fDiHyperEdge.map(f => (_, sources: OneOrMore[NN], targets: OneOrMore[NN]) => f(sources, targets)),
      fEdge.map(f => (_, n1: NN, n2: NN) => f(n1, n2))
    )

  /** $mapEdges
    *
    * $mapGeneric Otherwise see `mapDiHyperBound`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode $fNode
    * @param fDiHyperEdge $fDiHyperEdge
    * @param fEdge $fEdgeOption
    * @return $mapReturn
    */
  /* @param w witnesses that this graph is defined as a directed hypergraph by its `E` type parameter.
   */
  def mapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fDiHyperEdge: (EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC[NN],
      fEdge: Option[(EdgeT, NN, NN) => EC[NN]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]]

  /** $mapEdges
    *
    * $mapGeneric Otherwise see `mapDiHyperBound`.
    *
    * $seeOverload
    *
    * @param fDiHyperEdge $simplifiedFEdge
    */
  final def mapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fDiHyperEdge: (OneOrMore[NN], OneOrMore[NN]) => EC[NN],
      fEdge: Option[(NN, NN) => EC[NN]] = None
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]] =
    mapDiHyper(
      fNode,
      (_, sources: OneOrMore[NN], targets: OneOrMore[NN]) => fDiHyperEdge(sources, targets),
      fEdge.map(f => (_, n1: NN, n2: NN) => f(n1, n2))
    )

  /** $mapEdges
    *
    * $mapTyped Otherwise see `mapDiHyper`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode $fNode
    * @param fDiHyperEdge $fDiHyperEdge
    * @param fEdge $fEdgeOption
    * @return $mapReturn
    */
  /* @param w witnesses that this graph is defined as a directed hypergraph by its `E` type parameter.
   */
  def mapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fDiHyperEdge: (EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC,
      fEdge: Option[(EdgeT, NN, NN) => EC]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC]

  /** $mapEdges
    *
    * $mapTyped Otherwise see `mapDiHyper`.
    *
    * $seeOverload
    *
    * @param fDiHyperEdge $simplifiedFEdge
    */
  final def mapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fDiHyperEdge: (OneOrMore[NN], OneOrMore[NN]) => EC,
      fEdge: Option[(NN, NN) => EC] = None
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC] =
    mapDiHyperBound(
      fNode,
      (_, sources: OneOrMore[NN], targets: OneOrMore[NN]) => fDiHyperEdge(sources, targets),
      fEdge.map(f => (_, n1: NN, n2: NN) => f(n1, n2))
    )

  /** $flatMapNodes
    *
    * $mapGeneric Otherwise see `flatMapBound`.
    *
    * If this graph also contains typed edges, the typed edge's partial `map` function will be called to replace the ends.
    * If the partial function is not defined, there will be an attempt to fall back to a generic edge.
    * If that attempt also fails the edge will be dropped.
    * So, if you have a mixed graph with generic and typed edges, prefer mapping edges directly to avoid leaving edges out.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode $fNode $fNodeFlat
    * @return $flatMapReturn
    */
  /* @param w1    Ensures that the type parameter `E` of this graph is of type `GenericMapper`.
   * @param w2    Captures the higher kind of current `E` to determine the return type.
   */
  def flatMap[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN]
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, t: ClassTag[EC[NN]]): CC[NN, EC[NN]]

  /** $flatMapNodes
    *
    * $mapTyped Otherwise see `flatMap`.
    *
    * @param fNode $fNode $fNodeFlat
    */
  /* @param w1 Ensures that the type parameter `E` of this graph is of type `PartialMapper`.
   */
  def flatMapBound(fNode: NodeT => Seq[N])(implicit w1: E <:< PartialMapper): CC[N, E]

  /** $flatMapEdges
    *
    * $mapGeneric Otherwise see `flatMapBound`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode $fNode
    * @param fEdge $fEdge
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a non-hypergraph by its `E` type parameter.
   */
  def flatMap[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC[NN]]

  /** $flatMapEdges
    *
    * $seeOverload
    *
    * @param fEdge $simplifiedFEdge
    */
  final def flatMap[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fEdge: (Seq[NN], Seq[NN]) => Seq[EC[NN]]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC[NN]] =
    flatMap(fNode, (_, n1: Seq[NN], n2: Seq[NN]) => fEdge(n1, n2))

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMap`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode $fNode
    * @param fEdge $fEdge
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a non-hypergraph by its `E` type parameter.
   */
  def flatMapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC]

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMap`.
    *
    * $seeOverload
    *
    * @param fEdge $simplifiedFEdge
    */
  final def flatMapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fEdge: (Seq[NN], Seq[NN]) => Seq[EC]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC] =
    flatMapBound(fNode, (_, n1: Seq[NN], n2: Seq[NN]) => fEdge(n1, n2))

  /** $flatMapEdges
    *
    * $mapGeneric Otherwise see `flatMapHyperBound`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode        $fNode
    * @param fHyperEdge   $fHyperEdge
    * @param fDiHyperEdge $fDiHyperEdgeOption
    * @param fEdge        $fEdgeOption
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a hypergraph by its `E` type parameter.
   */
  def flatMapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: (EdgeT, Seq[NN]) => Seq[EC[NN]],
      fDiHyperEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]]

  /** $flatMapEdges
    *
    * $mapGeneric Otherwise see `flatMapHyperBound`.
    *
    * $seeOverload
    *
    * @param fHyperEdge $simplifiedFEdge
    */
  final def flatMapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: Seq[NN] => Seq[EC[NN]],
      fDiHyperEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC[NN]]] = None,
      fEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC[NN]]] = None
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]] =
    flatMapHyper(
      fNode,
      (_, ends: Seq[NN]) => fHyperEdge(ends),
      fDiHyperEdge.map(f => (_, sources: Seq[NN], targets: Seq[NN]) => f(sources, targets)),
      fEdge.map(f => (_, n1s: Seq[NN], n2s: Seq[NN]) => f(n1s, n2s))
    )

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMapHyper`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode        $fNode
    * @param fHyperEdge   $fHyperEdge
    * @param fDiHyperEdge $fDiHyperEdgeOption
    * @param fEdge        $fEdgeOption
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a hypergraph by its `E` type parameter.
   */
  def flatMapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: (EdgeT, Seq[NN]) => Seq[EC],
      fDiHyperEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC]

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMapHyper`.
    *
    * $seeOverload
    *
    * @param fHyperEdge $simplifiedFEdge
    */
  final def flatMapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: Seq[NN] => Seq[EC],
      fDiHyperEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC]] = None,
      fEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC]] = None
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC] =
    flatMapHyperBound(
      fNode,
      (_, ends: Seq[NN]) => fHyperEdge(ends),
      fDiHyperEdge.map(f => (_, sources: Seq[NN], targets: Seq[NN]) => f(sources, targets)),
      fEdge.map(f => (_, n1s: Seq[NN], n2s: Seq[NN]) => f(n1s, n2s))
    )

  /** $flatMapEdges
    *
    * $mapGeneric Otherwise see `flatMapDiHyperBound`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapEC
    * @param fNode        $fNode
    * @param fDiHyperEdge $fDiHyperEdge
    * @param fEdge        $fEdgeOption
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a directed hypergraph by its `E` type parameter.
   */
  def flatMapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]]

  /** $flatMapEdges
    *
    * $mapGeneric Otherwise see `flatMapDiHyperBound`.
    *
    * $seeOverload
    *
    * @param fDiHyperEdge $simplifiedFEdge
    */
  final def flatMapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (Seq[NN], Seq[NN]) => Seq[EC[NN]],
      fEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC[NN]]] = None
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]] =
    flatMapDiHyper(
      fNode,
      (_, sources: Seq[NN], targets: Seq[NN]) => fDiHyperEdge(sources, targets),
      fEdge.map(f => (_, n1s: Seq[NN], n2s: Seq[NN]) => f(n1s, n2s))
    )

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMapDiHyper`.
    *
    * @tparam NN $mapNN
    * @tparam EC $mapECTyped
    * @param fNode        $fNode
    * @param fDiHyperEdge $fDiHyperEdge
    * @param fEdge        $fEdgeOption
    * @return $flatMapReturn
    */
  /* @param w witnesses that this graph is defined as a directed hypergraph by its `E` type parameter.
   */
  def flatMapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC]

  /** $flatMapEdges
    *
    * $mapTyped Otherwise see `flatMapDiHyper`.
    *
    * $seeOverload
    *
    * @param fDiHyperEdge $simplifiedFEdge
    */
  final def flatMapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (Seq[NN], Seq[NN]) => Seq[EC],
      fEdge: Option[(Seq[NN], Seq[NN]) => Seq[EC]] = None
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC] =
    flatMapDiHyperBound(
      fNode,
      (_, sources: Seq[NN], targets: Seq[NN]) => fDiHyperEdge(sources, targets),
      fEdge.map(f => (_, n1s: Seq[NN], n2s: Seq[NN]) => f(n1s, n2s))
    )

  /** Applies a node-specific and an edge-specific binary operator to a cumulated value.
    * First `opNode` is called for all nodes than `opEdge` for all edges.
    *
    * @param z  the start value that is passed to `opNode` the first time.
    * @param opNode the binary operator that is passed the cumulated value and an inner node.
    * @param opEdge the binary operator that is passed the cumulated value and an inner edge.
    * @tparam B the result type of the binary operator.
    * @return the cumulated value.
    */
  def foldLeft[B](z: B)(opNode: (B, NodeT) => B, opEdge: (B, EdgeT) => B): B

  /** Same as `foldLeft` except the second parameter of the binary operators.
    *
    * @param opNode the binary operator that is passed the cumulated value and an outer node.
    * @param opEdge the binary operator that is passed the cumulated value and an outer edge.
    */
  final def foldLeftOuter[B](z: B)(opNode: (B, N) => B, opEdge: (B, E) => B): B =
    foldLeft(z)((cum: B, n: NodeT) => opNode(cum, n.outer), (cum: B, e: EdgeT) => opEdge(cum, e.outer))
}
