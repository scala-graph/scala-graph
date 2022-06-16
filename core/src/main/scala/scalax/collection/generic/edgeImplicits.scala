package scalax.collection.generic

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
