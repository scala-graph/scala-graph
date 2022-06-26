package scalax.collection.generic

import scala.collection.immutable.Iterable

object AbstractHyperEdgeImplicits {

  trait FromAny[N, E[N] <: AnyHyperEdge[N], C <: HyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def n1: N
    def ~~[NN >: N](n2: NN): E[NN] = companion(n1, n2)
  }

  trait FromEdge[N, E[N] <: AnyHyperEdge[N], C <: HyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def e1: E[N]
    def ~~[NN >: N](n: NN): E[NN] = companion.unsafeFrom(e1.ends ++ (n :: Nil))
  }
}

object AbstractDiHyperEdgeImplicits {

  trait FromAny[N, E[N] <: AnyDiHyperEdge[N], C <: DiHyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def source: N

    def ~~>[NN >: N](target: NN): E[NN] = companion[NN](source)(target)

    /** @throws IllegalArgumentException if targets is empty */
    def ~~>[NN >: N](targets: Iterable[NN]): E[NN] = companion.unsafeFrom(source :: Nil, targets)
  }

  trait FromIterable[N, E[N] <: AnyDiHyperEdge[N], C <: DiHyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def sources: Iterable[N]

    def ~~>[NN >: N](target: NN): E[NN] = companion.unsafeFrom(sources, target :: Nil)

    /** @throws IllegalArgumentException if targets is empty */
    def ~~>[NN >: N](targets: Iterable[NN]): E[NN] = companion.unsafeFrom(sources, targets)
  }
}

trait AbstractEdgeImplicits[N, E[X] <: Edge[X] with AnyEdge[X], C <: EdgeCompanion[E]] extends Any {
  protected def companion: C
  def n1: N
  def ~[NN >: N](n2: NN): E[NN] = companion(n1, n2)
}

trait AbstractDiEdgeImplicits[N, E[N] <: AnyDiEdge[N], C <: EdgeCompanion[E]] extends Any {
  protected def companion: C
  def source: N
  def ~>[NN >: N](target: NN): E[NN] = companion(source, target)
}
