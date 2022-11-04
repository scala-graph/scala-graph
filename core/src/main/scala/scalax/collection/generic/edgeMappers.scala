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

trait GenericHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with HyperEdgeMapper { this: AnyHyperEdge[_] =>
  def map[N](ends: Several[N]): CC[N]
}

trait GenericDiHyperEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with DiHyperEdgeMapper {
  this: AnyDiHyperEdge[_] =>
  def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): CC[N]
}

/** Mixin for directed and undirected generic edges to facilitate `Graph` mapping by `def map(fNode)`.
  *
  * @tparam CC type constructor of the final edge class.
  */
trait GenericEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N](node_1: N, node_2: N): CC[N]
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
