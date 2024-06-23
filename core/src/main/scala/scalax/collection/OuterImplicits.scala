package scalax.collection

import scala.language.implicitConversions
import scala.collection.{AbstractIterable, AbstractIterator, SeqFacade}

import scalax.collection.generic.Edge

object OuterImplicits {
  implicit def nodeSetToOuter[N, E <: Edge[N]](nodes: AnyGraph[N, E]#NodeSetT): Iterable[OuterNode[N]] =
    new AbstractIterable[OuterNode[N]] {
      def iterator: Iterator[OuterNode[N]] = new AbstractIterator[OuterNode[N]] {
        private[this] val it = nodes.iterator

        def hasNext = it.hasNext

        def next() = it.next().outer
      }
    }

  implicit def nodeSetToSeq[N, E <: Edge[N]](nodes: AnyGraph[N, E]#NodeSetT): Seq[AnyGraph[N, E]#NodeT] =
    new SeqFacade(nodes)

  implicit def edgeSetToOuter[N, E <: Edge[N]](edges: AnyGraph[N, E]#EdgeSetT): Iterable[OuterEdge[N, E]] =
    new AbstractIterable[OuterEdge[N, E]] {
      def iterator: Iterator[OuterEdge[N, E]] = new AbstractIterator[OuterEdge[N, E]] {
        private[this] val it = edges.iterator

        def hasNext = it.hasNext

        def next() = OuterEdge[N, E](it.next().outer)
      }
    }

  implicit def edgeSetToSeq[N, E <: Edge[N]](edges: AnyGraph[N, E]#EdgeSetT): Seq[AnyGraph[N, E]#EdgeT] =
    new SeqFacade(edges)

  @inline implicit def anyToNode[N](n: N): OuterNode[N] = OuterNode(n)

  implicit class SeqEnrichments[N, S[X] <: Seq[X]](private val seq: S[N]) extends AnyVal {
    def toOuterElems[E <: Edge[N]]: Seq[OuterElem[N, E]] = seq map anyToNode[N]
  }

  @inline implicit def toOuterEdge[N, E[X] <: Edge[X]](e: E[N]): OuterEdge[N, E[N]] = OuterEdge[N, E[N]](e)

  @inline implicit def toOuterEdge[N, E[X] <: Edge[X]](es: Seq[E[N]]): Seq[OuterEdge[N, E[N]]] =
    es map OuterEdge[N, E[N]]
}
