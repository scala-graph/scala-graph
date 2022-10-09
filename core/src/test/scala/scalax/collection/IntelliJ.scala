package scalax.collection

import scalax.collection.generic.Edge

trait IntelliJ[C[N, E <: Edge[N]] <: GraphLike[N, E, C] with AnyGraph[N, E]] {

  /** Moves lots of false positive IntelliJ errors on the higher kinded type parameter `C` to this single point.
    * Using this kind of IDE correction is not recommended in production code.
    */
  implicit class ForIntelliJ[N, E <: Edge[N]](val g: C[N, E]) {
    def asAnyGraph: AnyGraph[N, E] = g
  }
}
