package scalax.collection.hyperedges

import scalax.collection.generic.{AbstractDiHyperEdgeImplicits, AbstractHyperEdgeImplicits}

object HyperEdgeImplicits extends AbstractHyperEdgeImplicits[HyperEdge, HyperEdge.type](HyperEdge) {
  implicit class HyperAnyToEdge[N](override val n1: N)             extends AnyVal with AnyToEdge[N]
  implicit class HyperEdgeToEdge[N](override val e1: HyperEdge[N]) extends AnyVal with EdgeToEdge[N]
}

object OrderedHyperEdgeImplicits
    extends AbstractHyperEdgeImplicits[OrderedHyperEdge, OrderedHyperEdge.type](OrderedHyperEdge) {
  implicit class HyperAnyToEdge[N](override val n1: N)                    extends AnyVal with AnyToEdge[N]
  implicit class HyperEdgeToEdge[N](override val e1: OrderedHyperEdge[N]) extends AnyVal with EdgeToEdge[N]
}

object DiHyperEdgeImplicits extends AbstractDiHyperEdgeImplicits[DiHyperEdge, DiHyperEdge.type](DiHyperEdge) {
  implicit class DiHyperAnyToEdge[N](override val source: N)             extends AnyVal with AnyToEdge[N]
  implicit class DiHyperEdgeToEdge[N](override val sources: Iterable[N]) extends AnyVal with IterableToEdge[N]
}

object OrderedDiHyperEdgeImplicits
    extends AbstractDiHyperEdgeImplicits[OrderedDiHyperEdge, OrderedDiHyperEdge.type](OrderedDiHyperEdge) {
  implicit class DiHyperAnyToEdge[N](override val source: N)             extends AnyVal with AnyToEdge[N]
  implicit class DiHyperEdgeToEdge[N](override val sources: Iterable[N]) extends AnyVal with IterableToEdge[N]
}
