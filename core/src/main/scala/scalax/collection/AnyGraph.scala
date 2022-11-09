package scalax.collection

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.immutable.Iterable
import scala.reflect.ClassTag

import scalax.collection.generic._
import scalax.collection.generic.Factory
import scalax.collection.config.GraphConfig
import scalax.collection.edges.{DiEdge, UnDiEdge}
import scalax.collection.hyperedges.{ordered, DiHyperEdge, HyperEdge}
import scalax.collection.mutable.{Builder, EqHashMap}

/** A template trait for graphs.
  *
  * This trait provides the common structure and operations of immutable graphs independently
  * of their representation.
  *
  * If `E` inherits `DiHyperEdgeLike` the graph is directed, otherwise it is undirected or mixed.
  *
  * @tparam N    the user type of the nodes (vertices) in this graph.
  * @tparam E    the type of the edges in this graph.
  * @tparam This the higher kinded type of the graph itself.
  *
  * @define REIMPLFACTORY Note that this method must be reimplemented in each module
  *         having its own factory methods such as `constrained` does.
  * @define CONTGRAPH The `Graph` instance that contains `this`
  * @author Peter Empen
  */
trait GraphLike[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]]
    extends GraphBase[N, E, CC]
    with GraphTraversal[N, E]
    with GraphDegree[N, E, CC]
    with ToString[N, E, CC] {
  thisGraph: CC[N, E] =>

  protected type ThisGraph = thisGraph.type

  def empty: CC[N, E]
  protected[this] def newBuilder: mutable.Builder[N, E, CC]

  def isDirected: Boolean = edges.nonEmpty && edges.hasOnlyDiEdges
  def isHyper: Boolean    = edges.nonEmpty && edges.hasAnyHyperEdge
  def isMixed: Boolean    = edges.nonEmpty && edges.hasMixedEdges
  def isMulti: Boolean    = edges.nonEmpty && edges.hasAnyMultiEdge

  /** The companion object of `CC`. */
  val companion: Factory[CC]
  implicit def config: GraphConfig

  /** `Graph` instances are equal if their nodes and edges turned
    * to outer nodes and outer edges are equal. Any `TraversableOnce`
    * instance may also be equal to this graph if its set representation
    * contains equalling outer nodes and outer edges. Thus the following
    * expressions hold:
    * {{{
    * Graph(1~2, 3) == List(1~2, 3)
    * Graph(1~2, 3) == List(1, 2, 2, 3, 2~1)
    * }}}
    * The first test is `false` because of the failing nodes `1` and `2`.
    * The second is true because of duplicate elimination and undirected edge equivalence.
    */
  override def equals(that: Any): Boolean = that match {
    case that: AnyGraph[N, E] @unchecked =>
      (this eq that) ||
      this.order == that.order &&
      this.size == that.size && {
        val thatNodes = that.nodes.toOuter
        try this.nodes forall (thisN => thatNodes(thisN.outer))
        catch { case _: ClassCastException => false }
      } && {
        val thatEdges = that.edges.toOuter
        try this.edges forall (thisE => thatEdges(thisE.outer))
        catch { case _: ClassCastException => false }
      }
    case that: IterableOnce[_] =>
      val thatSet = that.iterator.to(Set)
      this.elementCount == thatSet.size && {
        val thatNodes = thatSet.asInstanceOf[Set[N]]
        try this.nodes forall (thisN => thatNodes(thisN.outer))
        catch { case _: ClassCastException => false }
      } && {
        val thatEdges = thatSet.asInstanceOf[Set[E]]
        try this.edges forall (thisE => thatEdges(thisE.outer))
        catch { case _: ClassCastException => false }
      }
    case _ =>
      false
  }

  type NodeT <: GraphInnerNode
  trait GraphInnerNode extends BaseInnerNode with TraverserInnerNode { this: NodeT =>

    /** $CONTGRAPH inner edge. */
    final def containingGraph: ThisGraph = thisGraph
  }

  protected trait NodeBase extends BaseNodeBase with GraphInnerNode {
    this: NodeT =>
    final def isContaining[N, E <: Edge[N]](g: GraphBase[N, E, CC] @uV): Boolean =
      g eq containingGraph
  }

  type NodeSetT <: GraphNodeSet
  trait GraphNodeSet extends NodeSet with NodeSetToString {
    protected def copy: NodeSetT

    final override def -(node: NodeT): NodeSetT =
      if (this contains node) { val c = copy; c minus node; c }
      else this.asInstanceOf[NodeSetT]

    /** removes `node` from this node set leaving the edge set unchanged.
      *
      * @param node the node to be removed from the node set.
      */
    protected def minus(node: NodeT): Unit

    /** removes `node` either rippling or gently.
      *
      * @param node the node to be subtracted
      * @param rippleDelete if `true`, `node` will be deleted with its incident edges;
      *        otherwise `node` will be only deleted if it has no incident edges or
      *        all its incident edges are hooks.
      * @param minusNode implementation of node removal without considering incident edges.
      * @param minusEdges implementation of removal of all incident edges.
      * @return `true` if `node` has been removed.
      */
    final protected[collection] def subtract(
        node: NodeT,
        rippleDelete: Boolean,
        minusNode: (NodeT) => Unit,
        minusEdges: (NodeT) => Unit
    ): Boolean = {
      def minusNodeTrue = { minusNode(node); true }
      def minusAllTrue  = { minusEdges(node); minusNodeTrue }
      if (contains(node))
        if (node.edges.isEmpty) minusNodeTrue
        else if (rippleDelete) minusAllTrue
        else if (node.hasOnlyHooks) minusAllTrue
        else handleNotGentlyRemovable
      else false
    }
    protected def handleNotGentlyRemovable = false
  }

  trait GraphInnerEdge extends BaseInnerEdge {
    this: EdgeT =>

    /** $CONTGRAPH inner edge. */
    final def containingGraph: ThisGraph = thisGraph

    final protected[collection] def asEdgeT: EdgeT = this
  }

  type EdgeT = GraphInnerEdge

  import scala.{SerialVersionUID => S}
  // format: off
  @S(-901) class InnerHyperEdge       (val ends: Several[NodeT], val outer: E) extends Abstract.HyperEdge       (ends) with EdgeT
  @S(-902) class InnerOrderedHyperEdge(val ends: Several[NodeT], val outer: E) extends Abstract.OrderedHyperEdge(ends) with EdgeT

  @S(-903) class InnerDiHyperEdge       (override val sources: OneOrMore[NodeT], override val targets: OneOrMore[NodeT], val outer: E) extends Abstract.DiHyperEdge       (sources, targets) with EdgeT
  @S(-904) class InnerOrderedDiHyperEdge(override val sources: OneOrMore[NodeT], override val targets: OneOrMore[NodeT], val outer: E) extends Abstract.OrderedDiHyperEdge(sources, targets) with EdgeT

  @S(-905) class InnerUnDiEdge(val source: NodeT, val target: NodeT, val outer: E) extends Abstract.UnDiEdge(source, target) with EdgeT
  @S(-906) class InnerDiEdge  (val source: NodeT, val target: NodeT, val outer: E) extends Abstract.DiEdge  (source, target) with EdgeT

  @transient object InnerHyperEdge        { def unapply(edge:        InnerHyperEdge): Option[Several[NodeT]] = Some(edge.ends) }
  @transient object InnerOrderedHyperEdge { def unapply(edge: InnerOrderedHyperEdge): Option[Several[NodeT]] = Some(edge.ends) }

  @transient object InnerDiHyperEdge        { def unapply(edge:        InnerDiHyperEdge): Option[(OneOrMore[NodeT], OneOrMore[NodeT])] = Some(edge.sources, edge.targets) }
  @transient object InnerOrderedDiHyperEdge { def unapply(edge: InnerOrderedDiHyperEdge): Option[(OneOrMore[NodeT], OneOrMore[NodeT])] = Some(edge.sources, edge.targets) }

  @transient object InnerUnDiEdge { def unapply(edge: InnerUnDiEdge): Option[(NodeT, NodeT)] = Some(edge.source, edge.target) }
  @transient object InnerDiEdge   { def unapply(edge:   InnerDiEdge): Option[(NodeT, NodeT)] = Some(edge.source, edge.target) }
  // format: on

  final override protected def newHyperEdge(outer: E, nodes: Several[NodeT]): EdgeT = outer match {
    case _: AnyHyperEdge[N] with OrderedEndpoints => new InnerOrderedHyperEdge(nodes, outer)
    case _: AnyHyperEdge[N]                       => new InnerHyperEdge(nodes, outer)
    case e                                        => throw new MatchError(s"Unexpected HyperEdge $e")
  }
  protected def newDiHyperEdge(outer: E, sources: OneOrMore[NodeT], targets: OneOrMore[NodeT]): EdgeT = outer match {
    case _: AnyDiHyperEdge[N] with OrderedEndpoints => new InnerOrderedDiHyperEdge(sources, targets, outer)
    case _: AnyDiHyperEdge[N]                       => new InnerDiHyperEdge(sources, targets, outer)
    case e                                          => throw new MatchError(s"Unexpected DiHyperEdge $e")
  }
  protected def newEdge(outer: E, node_1: NodeT, node_2: NodeT): EdgeT = outer match {
    case _: AnyDiEdge[N]   => new InnerDiEdge(node_1, node_2, outer)
    case _: AnyUnDiEdge[N] => new InnerUnDiEdge(node_1, node_2, outer)
    case e                 => throw new MatchError(s"Unexpected Edge $e")
  }

  type EdgeSetT <: GraphEdgeSet
  trait GraphEdgeSet extends EdgeSet with EdgeSetToString {
    def hasOnlyDiEdges: Boolean
    def hasOnlyUnDiEdges: Boolean
    def hasMixedEdges: Boolean
    def hasAnyHyperEdge: Boolean
    def hasAnyMultiEdge: Boolean
  }

  final def contains(node: N): Boolean = nodes contains newNode(node)
  final def contains(edge: E): Boolean = edges contains newHyperEdge(edge, edge.ends map newNode)

  def iterator: Iterator[InnerElem] = nodes.iterator ++ edges.iterator

  def toIterable: Iterable[InnerElem] = new Iterable[InnerElem] {
    def iterator: Iterator[InnerElem] = thisGraph.iterator
  }

  def outerIterator: Iterator[OuterElem] =
    nodes.iterator.map[OuterElem](n => OuterNode(n.outer)) ++
      edges.iterator.map[OuterElem](e => OuterEdge(e.outer))

  def toOuterIterable: Iterable[OuterElem] = new Iterable[OuterElem] {
    def iterator: Iterator[OuterElem] = outerIterator
  }

  @inline final def find(node: N): Option[NodeT] = nodes find node
  @inline final def find(edge: E): Option[EdgeT] = edges find edge

  @inline final def get(node: N): NodeT = nodes get node
  @inline final def get(edge: E): EdgeT = edges.find(edge).get

  final def filter(nodeP: NodePredicate = anyNode, edgeP: EdgePredicate = anyEdge): CC[N, E] = {
    import scala.collection.Set

    def build(nodes: Set[NodeT]): CC[N, E] = {
      val b = companion.newBuilder[N, E]
      nodes foreach { case InnerNode(innerN, outerN) =>
        b addOne outerN
        innerN.edges foreach { case InnerEdge(innerE, outerE) =>
          if (edgeP(innerE) && (innerE.ends forall nodes.contains)) b += outerE
        }
      }
      b.result
    }
    (nodeP, edgeP) match {
      case (`anyNode`, `anyEdge`) => this
      case (`anyNode`, _)         => build(nodes)
      case (fN, _)                => build(nodes filter fN)
    }
  }

  final def map[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, t: ClassTag[EC[NN]]): CC[NN, EC[NN]] =
    mapNodes[NN, EC[NN]](fNode) match {
      case (nMap, builder) => map(nMap, builder)
    }

  private def map[NN, EC[X] <: Edge[X]](
      nMap: EqHashMap[NodeT, NN],
      builder: Builder[NN, EC[NN], CC @uV]
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, t: ClassTag[EC[NN]]): CC[NN, EC[NN]] = {
    def validate[E <: Edge[NN]](e: E): Option[EC[NN]] = e match {
      case e if t.runtimeClass.isInstance(e) => Some(e.asInstanceOf[EC[NN]])
      case _                                 => None
    }
    edges foreach {
      case InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), outer) =>
        def fallback[A]: (A, A) => AnyEdge[A] = outer match {
          case _: AnyDiEdge[N] => DiEdge.apply
          case _               => UnDiEdge.apply
        }

        (nMap get n1, nMap get n2) match {
          case (Some(nn1), Some(nn2)) =>
            outer match {
              case gM: GenericEdgeMapper[EC @unchecked] => builder += gM.map(nn1, nn2)
              case pM: PartialEdgeMapper[EC[NN] @unchecked] =>
                pM.map[NN].lift(nn1, nn2).fold(validate(fallback(nn1, nn2)))(Some(_)).map(builder += _)
            }
          case _ =>
        }
      case InnerEdge(
            AnyDiHyperEdge(sources: OneOrMore[NodeT] @unchecked, targets: OneOrMore[NodeT] @unchecked),
            outer
          ) =>
        def fallback[A]: (OneOrMore[A], OneOrMore[A]) => AnyDiHyperEdge[A] = outer match {
          case _: OrderedEndpoints => ordered.DiHyperEdge.apply
          case _                   => DiHyperEdge.apply
        }

        (sources.flatMapEither(nMap.get), targets.flatMapEither(nMap.get)) match {
          case (Right(newSources), Right(newTargets)) =>
            outer match {
              case gM: GenericDiHyperEdgeMapper[EC @unchecked] => builder += gM.map(newSources, newTargets)
              case pM: PartialDiHyperEdgeMapper[EC[NN] @unchecked] =>
                pM.map[NN]
                  .lift(newSources, newTargets)
                  .fold(validate(fallback(newSources, newTargets)))(Some(_))
                  .map(builder += _)
            }
          case _ =>
        }
      case InnerEdge(AnyHyperEdge(ends: Several[NodeT] @unchecked), outer) =>
        def fallback[A]: Several[A] => AnyHyperEdge[A] = outer match {
          case _: OrderedEndpoints => ordered.HyperEdge.apply
          case _                   => HyperEdge.apply
        }

        ends.flatMapEither(nMap.get) match {
          case Right(newEnds) =>
            outer match {
              case gM: GenericHyperEdgeMapper[EC @unchecked] => builder += gM.map(newEnds)
              case pM: PartialHyperEdgeMapper[EC[NN] @unchecked] =>
                pM.map[NN].lift(newEnds).fold(validate(fallback(newEnds)))(Some(_)).map(builder += _)
            }
          case _ =>
        }
    }
    builder.result
  }

  final def mapBound(fNode: NodeT => N)(implicit w1: E <:< PartialMapper): CC[N, E] =
    mapNodes[N, E](fNode) match {
      case (nMap, builder) => mapBound(nMap, builder)
    }

  private def mapBound(nMap: EqHashMap[NodeT, N], builder: Builder[N, E, CC @uV]): CC[N, E] = {
    edges foreach {
      case InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), outer) =>
        (nMap get n1, nMap get n2) match {
          case (Some(nn1), Some(nn2)) =>
            outer match {
              case pM: PartialEdgeMapper[E @unchecked] => pM.map[N].lift(nn1, nn2).map(builder += _)
              case _                                   =>
            }
          case _ =>
        }
      case InnerEdge(
            AnyDiHyperEdge(sources: OneOrMore[NodeT] @unchecked, targets: OneOrMore[NodeT] @unchecked),
            outer
          ) =>
        (sources.flatMapEither(nMap.get), targets.flatMapEither(nMap.get)) match {
          case (Right(newSources), Right(newTargets)) =>
            outer match {
              case pM: PartialDiHyperEdgeMapper[E @unchecked] =>
                pM.map[N].lift(newSources, newTargets).map(builder += _)
              case _ =>
            }
          case _ =>
        }
      case InnerEdge(AnyHyperEdge(ends: Several[NodeT] @unchecked), outer) =>
        ends.flatMapEither(nMap.get) match {
          case Right(newEnds) =>
            outer match {
              case pM: PartialHyperEdgeMapper[E @unchecked] => pM.map[N].lift(newEnds).map(builder += _)
              case _                                        =>
            }
          case _ =>
        }
    }
    builder.result
  }

  private def mapNodes[NN, EE <: Edge[NN]](fNode: NodeT => NN): (EqHashMap[NodeT, NN], Builder[NN, EE, CC]) = {
    val nMap = EqHashMap.empty[NodeT, NN](order)
    val b    = companion.newBuilder[NN, EE](config)
    nodes foreach { n =>
      val nn = fNode(n)
      nMap put (n, nn)
      b += nn
    }
    (nMap, b)
  }

  private def flatMapNodesBiased[NN, EE <: Edge[NN]](
      fNode: NodeT => Seq[NN]
  ): (EqHashMap[NodeT, NN], Builder[NN, EE, CC]) = {
    val nMap = EqHashMap.empty[NodeT, NN](order)
    val b    = companion.newBuilder[NN, EE](config)
    nodes foreach { n =>
      val newNodes = fNode(n)
      newNodes.headOption.map { head =>
        var preserved: Option[NN] = None
        newNodes foreach { nn =>
          b += nn
          if (n.outer == nn) preserved = Some(nn)
        }
        nMap put (n, preserved.fold(head)(identity))
      }
    }
    (nMap, b)
  }

  private def flatMapNodes[NN, EE <: Edge[NN]](
      fNode: NodeT => Seq[NN]
  ): (EqHashMap[NodeT, Seq[NN]], Builder[NN, EE, CC]) = {
    val nMap = EqHashMap.empty[NodeT, Seq[NN]](order)
    val b    = companion.newBuilder[NN, EE](config)
    nodes foreach { n =>
      val newNodes = fNode(n)
      nMap put (n, newNodes)
      b ++= newNodes
    }
    (nMap, b)
  }

  final def map[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fEdge: (EdgeT, NN, NN) => EC[NN]
  )(implicit
      w: E <:< AnyEdge[N]
  ): CC[NN, EC[NN]] =
    mapBound(fNode, fEdge)

  final def mapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fEdge: (EdgeT, NN, NN) => EC
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC] =
    mapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach { case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
          builder += fEdge(e, nMap(n1), nMap(n2))
        }
        builder.result
    }

  final def mapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fHyperEdge: (EdgeT, Several[NN]) => EC[NN],
      fDiHyperEdge: Option[(EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC[NN]],
      fEdge: Option[(EdgeT, NN, NN) => EC[NN]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]] =
    mapHyperBound(fNode, fHyperEdge, fDiHyperEdge, fEdge)

  final def mapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fHyperEdge: (EdgeT, Several[NN]) => EC,
      fDiHyperEdge: Option[(EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC],
      fEdge: Option[(EdgeT, NN, NN) => EC]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC] =
    mapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach {
          case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
            fEdge.fold {
              builder += fHyperEdge(e, Several(nMap(n1), nMap(n2)))
            } { f =>
              builder += f(e, nMap(n1), nMap(n2))
            }
          case e @ InnerEdge(
                AnyDiHyperEdge(sources: OneOrMore[NodeT @unchecked], targets: OneOrMore[NodeT @unchecked]),
                _
              ) =>
            fDiHyperEdge.fold {
              builder += fHyperEdge(e, Several.fromUnsafe(sources.map(nMap).iterator ++ targets.map(nMap).iterator))
            } { f =>
              builder += f(e, sources.map(nMap), targets.map(nMap))
            }
          case e @ InnerEdge(AnyHyperEdge(ends: Several[NodeT @unchecked]), _) =>
            builder += fHyperEdge(e, ends.map(nMap))
        }
        builder.result
    }

  def mapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => NN,
      fDiHyperEdge: (EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC[NN],
      fEdge: Option[(EdgeT, NN, NN) => EC[NN]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]] =
    mapDiHyperBound(fNode, fDiHyperEdge, fEdge)

  def mapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => NN,
      fDiHyperEdge: (EdgeT, OneOrMore[NN], OneOrMore[NN]) => EC,
      fEdge: Option[(EdgeT, NN, NN) => EC]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC] =
    mapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach {
          case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
            import OneOrMore.one
            fEdge.fold {
              builder += fDiHyperEdge(e, one(nMap(n1)), one(nMap(n2)))
            } { f =>
              builder += f(e, nMap(n1), nMap(n2))
            }
          case e @ InnerEdge(
                AnyDiHyperEdge(sources: OneOrMore[NodeT @unchecked], targets: OneOrMore[NodeT @unchecked]),
                _
              ) =>
            builder += fDiHyperEdge(e, sources.map(nMap), targets.map(nMap))
        }
        builder.result
    }

  final def flatMap[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN]
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, t: ClassTag[EC[NN]]): CC[NN, EC[NN]] =
    flatMapNodesBiased[NN, EC[NN]](fNode) match {
      case (nMap, builder) => map(nMap, builder)
    }

  final def flatMapBound(fNode: NodeT => Seq[N])(implicit w1: E <:< PartialMapper): CC[N, E] =
    flatMapNodesBiased[N, E](fNode) match {
      case (nMap, builder) => mapBound(nMap, builder)
    }

  final def flatMap[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC[NN]] =
    flatMapBound(fNode, fEdge)

  final def flatMapBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC]
  )(implicit w: E <:< AnyEdge[N]): CC[NN, EC] =
    flatMapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach { case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
          builder ++= (edges = fEdge(e, nMap(n1), nMap(n2)))
        }
        builder.result
    }

  final def flatMapHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: (EdgeT, Seq[NN]) => Seq[EC[NN]],
      fDiHyperEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC[NN]] =
    flatMapHyperBound(fNode, fHyperEdge, fDiHyperEdge, fEdge)

  final def flatMapHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fHyperEdge: (EdgeT, Seq[NN]) => Seq[EC],
      fDiHyperEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]]
  )(implicit w: E <:< AnyHyperEdge[N]): CC[NN, EC] =
    flatMapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach {
          case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
            val nn1s = nMap(n1)
            val nn2s = nMap(n2)
            builder ++= (edges = fEdge.fold(fHyperEdge(e, nn1s ++ nn2s))(_(e, nn1s, nn2s)))

          case e @ InnerEdge(
                AnyDiHyperEdge(sources: OneOrMore[NodeT @unchecked], targets: OneOrMore[NodeT @unchecked]),
                _
              ) =>
            val newSources = sources.flatMap(nMap)
            val newTargets = targets.flatMap(nMap)
            builder ++= (edges =
              fDiHyperEdge.fold(fHyperEdge(e, newSources ++ newTargets))(_(e, newSources, newTargets)))

          case e @ InnerEdge(AnyHyperEdge(ends: Several[NodeT @unchecked]), _) =>
            builder ++= (edges = fHyperEdge(e, ends.flatMap(nMap)))
        }
        builder.result
    }

  final def flatMapDiHyper[NN, EC[X] <: Edge[X]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC[NN]]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC[NN]] =
    flatMapDiHyperBound(fNode, fDiHyperEdge, fEdge)

  final def flatMapDiHyperBound[NN, EC <: Edge[NN]](
      fNode: NodeT => Seq[NN],
      fDiHyperEdge: (EdgeT, Seq[NN], Seq[NN]) => Seq[EC],
      fEdge: Option[(EdgeT, Seq[NN], Seq[NN]) => Seq[EC]]
  )(implicit w: E <:< AnyDiHyperEdge[N]): CC[NN, EC] =
    flatMapNodes[NN, EC](fNode) match {
      case (nMap, builder) =>
        edges foreach {
          case e @ InnerEdge(AnyEdge(n1: NodeT @unchecked, n2: NodeT @unchecked), _) =>
            val nn1s = nMap(n1)
            val nn2s = nMap(n2)
            builder ++= (edges = fEdge.fold(fDiHyperEdge(e, nn1s, nn2s))(_(e, nn1s, nn2s)))

          case e @ InnerEdge(
                AnyDiHyperEdge(sources: OneOrMore[NodeT @unchecked], targets: OneOrMore[NodeT @unchecked]),
                _
              ) =>
            val newSources = sources.flatMap(nMap)
            val newTargets = targets.flatMap(nMap)
            builder ++= (edges = fDiHyperEdge(e, newSources, newTargets))
        }
        builder.result
    }
}

/** Bundled functionality for mutable and immutable graphs alike.
  *
  * @tparam N the type of the nodes (vertices) in this graph.
  * @tparam E the type of the edges in this graph.
  */
trait AnyGraph[N, E <: Edge[N]] extends GraphLike[N, E, AnyGraph]
