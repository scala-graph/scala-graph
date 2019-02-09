package scalax.collection

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, implicitConversions}
import scala.collection.{AbstractIterable, SeqFacade}

import GraphEdge._

/** This object serves as a container for several `Graph`-related definitions like parameter-types and implicit conversions.
  *
  * @author Peter Empen
  */
object GraphPredef {

  /** Represents parameters that are accepted when calling `Graph(...)`.
    *
    * @tparam N  the type of the nodes (vertices)
    * @tparam E  the kind of the edges (links)
    */
  sealed trait OuterElem[+N, +E <: EdgeLike[N]]

  /** Wraps any type to be accepted when calling `Graph(...)`. */
  sealed case class OuterNode[+N](node: N) extends OuterElem[N, Nothing]

  /** To be mixed in by edge classes to allow passing them to `Graph(...)`. */
  sealed case class OuterEdge[N, E <: EdgeLike[N]](edge: E) extends OuterElem[N, E]

  implicit def nodeSetToOuter[N, E <: EdgeLike[N]](nodes: Graph[N, E]#NodeSetT): Iterable[OuterNode[N]] =
    new AbstractIterable[OuterNode[N]] {
      def iterator = new AbstractIterator[OuterNode[N]] {
        private[this] val it = nodes.iterator
        def hasNext          = it.hasNext
        def next             = it.next.outer
      }
    }

  implicit def nodeSetToSeq[N, E <: EdgeLike[N]](nodes: Graph[N, E]#NodeSetT): Seq[Graph[N, E]#NodeT] =
    new SeqFacade(nodes)

  implicit def edgeSetToOuter[N, E <: EdgeLike[N]](edges: Graph[N, E]#EdgeSetT): Iterable[OuterEdge[N, E]] =
    new AbstractIterable[OuterEdge[N, E]] {
      def iterator = new AbstractIterator[OuterEdge[N, E]] {
        private[this] val it = edges.iterator
        def hasNext          = it.hasNext
        def next             = OuterEdge[N, E](it.next.outer)
      }
    }

  implicit def edgeSetToSeq[N, E <: EdgeLike[N]](edges: Graph[N, E]#EdgeSetT): Seq[Graph[N, E]#EdgeT] =
    new SeqFacade(edges)

  @inline implicit def anyToNode[N](n: N): OuterNode[N] = OuterNode(n)

  implicit class SeqEnrichments[N, S[X] <: Seq[X]](private val seq: S[N]) extends AnyVal {
    def toOuterElems[E <: EdgeLike[N]]: Seq[OuterElem[N, E]] = seq map anyToNode[N]
  }

  @inline implicit def toOuterEdge[N, E[X] <: EdgeLike[X]](e: E[N]): OuterEdge[N, E[N]] = OuterEdge[N, E[N]](e)

  @inline implicit def toOuterEdge[N, E[X] <: EdgeLike[X]](es: Seq[E[N]]): Seq[OuterEdge[N, E[N]]] =
    es map OuterEdge[N, E[N]]

  abstract class AbstractHyperEdgeImplicits[E[N] <: AnyHyperEdge[N], C <: HyperEdgeCompanion[E]](companion: C) {

    trait AnyToEdge[N] extends Any {
      def n1: N
      def ~~[NN >: N](n2: NN): E[NN] = companion(n1, n2)
    }

    trait EdgeToEdge[N] extends Any {
      def e1: E[N]
      def ~~[NN >: N](n: NN): E[NN] = companion[NN](e1.ends ++ (n :: Nil))
    }
  }

  object HyperEdgeImplicits extends AbstractHyperEdgeImplicits[HyperEdge, HyperEdge.type](HyperEdge) {
    implicit class AnyToEdge[N](override val n1: N)             extends AnyVal with super.AnyToEdge[N]
    implicit class EdgeToEdge[N](override val e1: HyperEdge[N]) extends AnyVal with super.EdgeToEdge[N]
  }

  object OrderedHyperEdgeImplicits
      extends AbstractHyperEdgeImplicits[OrderedHyperEdge, OrderedHyperEdge.type](OrderedHyperEdge) {
    implicit class AnyToEdge[N](override val n1: N)                    extends AnyVal with super.AnyToEdge[N]
    implicit class EdgeToEdge[N](override val e1: OrderedHyperEdge[N]) extends AnyVal with super.EdgeToEdge[N]
  }

  abstract class AbstractDiHyperEdgeImplicits[E[N] <: AnyDiHyperEdge[N], C <: DiHyperEdgeCompanion[E]](companion: C) {

    trait AnyToEdge[N] extends Any {
      def source: N
      def ~~>[NN >: N](target: NN): E[NN]            = companion(Iterable(source), Iterable(target))
      def ~~>[NN >: N](targets: Iterable[NN]): E[NN] = companion(Iterable(source), targets)
    }

    trait IterableToEdge[N] extends Any {
      def sources: Iterable[N]
      def ~~>[NN >: N](target: NN): E[NN]            = companion(sources, Iterable(target))
      def ~~>[NN >: N](targets: Iterable[NN]): E[NN] = companion(sources, targets)
    }
  }

  object DiHyperEdgeImplicits extends AbstractDiHyperEdgeImplicits[DiHyperEdge, DiHyperEdge.type](DiHyperEdge) {
    implicit class AnyToEdge[N](override val source: N)             extends AnyVal with super.AnyToEdge[N]
    implicit class EdgeToEdge[N](override val sources: Iterable[N]) extends AnyVal with super.IterableToEdge[N]
  }

  object OrderedDiHyperEdgeImplicits
      extends AbstractDiHyperEdgeImplicits[OrderedDiHyperEdge, OrderedDiHyperEdge.type](OrderedDiHyperEdge) {
    implicit class AnyToEdge[N](override val source: N)             extends AnyVal with super.AnyToEdge[N]
    implicit class EdgeToEdge[N](override val sources: Iterable[N]) extends AnyVal with super.IterableToEdge[N]
  }

  trait AbstractEdgeImplicits[N, E[X] <: EdgeLike[X] with AnyEdge[X], C <: EdgeCompanion[E]] extends Any {
    protected def companion: C
    def n1: N
    def ~[NN >: N](n2: NN): E[NN] = companion(n1, n2)
  }

  implicit class UnDiEdgeImplicits[N](override val n1: N)
      extends AnyVal
      with AbstractEdgeImplicits[N, UnDiEdge, UnDiEdge.type] {
    protected def companion = UnDiEdge
  }

  trait AbstractDiEdgeImplicits[N, E[N] <: AnyDiEdge[N], C <: EdgeCompanion[E]] extends Any {
    protected def companion: C
    def source: N
    def ~>[NN >: N](target: NN): E[NN] = companion(source, target)
  }

  implicit final class DiEdgeImplicits[N](val source: N)
      extends AnyVal
      with AbstractDiEdgeImplicits[N, DiEdge, DiEdge.type] {
    protected def companion = DiEdge
  }
}
