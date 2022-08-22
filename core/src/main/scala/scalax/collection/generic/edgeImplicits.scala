package scalax.collection
package generic

import scala.collection.immutable.Iterable

object AbstractHyperEdgeImplicits {

  trait FromAny[N, E[N] <: AbstractHyperEdge[N], C <: HyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def n1: N
    def ~~[NN >: N](n2: NN): E[NN] = companion(n1, n2)
  }

  trait FromEdge[N, E[N] <: AbstractHyperEdge[N], C <: HyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def e1: E[N]
    def ~~[NN >: N](n: NN): E[NN] = companion(Several(e1._1, e1._2, (e1.ends.more: Iterable[NN]) ++ (n :: Nil)))
  }
}

object AbstractDiHyperEdgeImplicits {

  trait FromOneOrMore[N, E[N] <: AbstractDiHyperEdge[N], C <: DiHyperEdgeCompanion[E]] extends Any {
    protected def companion: C
    def sources: OneOrMore[N]

    def ~~>[NN >: N](targets: OneOrMore[NN]): E[NN] = companion[NN](sources, targets)
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
