package scalax.collection
package generic

sealed protected[collection] trait Mapper
sealed protected[collection] trait GenericMapper extends Mapper
trait PartialMapper extends Mapper {
  def map[N]: PartialFunction[_, _]
}

sealed protected[collection] trait HyperEdgeMapper   extends Mapper
sealed protected[collection] trait DiHyperEdgeMapper extends Mapper
sealed protected[collection] trait EdgeMapper        extends Mapper

/** Mixin for undirected generic hyperedges to facilitate `Graph` mapping by `def map(fNode)`.
  *
  * @tparam CC type constructor of the final edge class.
  */
trait GenericHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with HyperEdgeMapper { this: AnyHyperEdge[_] =>
  def map[N](ends: Several[N]): CC[N]
}

/** Specialized `GenericHyperEdgeMapper` that implements `def map` in terms of `def copy`.
  */
trait GenericUnlabeledHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericHyperEdgeMapper[CC] {
  this: AnyHyperEdge[_] =>
  def copy[N](ends: Several[N]): CC[N]
  def map[N](ends: Several[N]): CC[N] = copy(ends)
}

/** Mixin for directed generic hyperedges to facilitate `Graph` mapping by `def map(fNode)`.
  *
  * @tparam CC type constructor of the final edge class.
  */
trait GenericDiHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with DiHyperEdgeMapper {
  this: AnyDiHyperEdge[_] =>
  def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): CC[N]
}

/** Specialized `GenericDiHyperEdgeMapper` that implements `def map` in terms of `def copy`.
  */
trait GenericUnlabeledDiHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericDiHyperEdgeMapper[CC] {
  this: AnyDiHyperEdge[_] =>
  def copy[N](sources: OneOrMore[N], targets: OneOrMore[N]): CC[N]
  def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): CC[N] = copy(sources, targets)
}

/** Mixin for directed and undirected generic edges to facilitate `Graph` mapping by `def map(fNode)`.
  *
  * @tparam CC type constructor of the final edge class.
  */
trait GenericEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N](node_1: N, node_2: N): CC[N]
}

/** Specialized `GenericEdgeMapper` that implements `def map` in terms of `def copy`.
  */
trait GenericUnlabeledEdgeMapper[+CC[X] <: Edge[X]] extends GenericEdgeMapper[CC] { this: AnyEdge[_] =>
  def copy[N](node_1: N, node_2: N): CC[N]
  def map[N](node_1: N, node_2: N): CC[N] = copy(node_1, node_2)
}

trait PartialHyperEdgeMapper[+CC <: Edge[_]] extends PartialMapper with HyperEdgeMapper { this: AnyHyperEdge[_] =>
  def map[N]: PartialFunction[Several[N], CC]
}

trait PartialDiHyperEdgeMapper[+CC <: Edge[_]] extends PartialMapper with DiHyperEdgeMapper {
  this: AnyDiHyperEdge[_] =>
  def map[N]: PartialFunction[(OneOrMore[N], OneOrMore[N]), CC]
}

trait PartialEdgeMapper[+CC <: Edge[_]] extends PartialMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N]: PartialFunction[(N, N), CC]
}
