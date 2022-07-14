package scalax.collection.generic

sealed protected[collection] trait Mapper
sealed protected[collection] trait GenericMapper extends Mapper
sealed protected[collection] trait PartialMapper extends Mapper

sealed protected[collection] trait HyperEdgeMapper   extends Mapper
sealed protected[collection] trait DiHyperEdgeMapper extends Mapper
sealed protected[collection] trait EdgeMapper        extends Mapper

trait GenericHyperEdgeMapper   extends GenericMapper with HyperEdgeMapper
trait GenericDiHyperEdgeMapper extends GenericMapper with DiHyperEdgeMapper

/** Mixin for directed and undirected generic edges to facilitate `Graph` mapping by `def map(fNode)`.
  *
  * @tparam CC type constructor of the final edge class.
  */
trait GenericEdgeMapper[+CC[X] <: Edge[X]] extends GenericMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N](node_1: N, node_2: N): CC[N]
}

trait PartialHyperEdgeMapper[+CC <: Edge[_]] extends PartialMapper with HyperEdgeMapper { this: AnyHyperEdge[_] =>
  def map[N]: PartialFunction[Iterable[N], CC]
}

trait PartialDiHyperEdgeMapper[+CC <: Edge[_]] extends PartialMapper with DiHyperEdgeMapper {
  this: AnyDiHyperEdge[_] =>
  def map[N]: PartialFunction[(Iterable[N], Iterable[N]), CC]
}

trait PartialEdgeMapper[+CC <: Edge[_]] extends PartialMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N]: PartialFunction[(N, N), CC]
}
