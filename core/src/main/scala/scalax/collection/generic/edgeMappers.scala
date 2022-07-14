package scalax.collection.generic

sealed protected[collection] trait Mapper
sealed protected[collection] trait GenericMapper extends Mapper
sealed protected[collection] trait PartialMapper extends Mapper

sealed protected[collection] trait HyperEdgeMapper   extends Mapper
sealed protected[collection] trait DiHyperEdgeMapper extends Mapper
sealed protected[collection] trait EdgeMapper        extends Mapper

sealed trait GenericHyperEdgeMapper   extends GenericMapper with HyperEdgeMapper
sealed trait GenericDiHyperEdgeMapper extends GenericMapper with DiHyperEdgeMapper

trait GenericEdgeMapper[+This[X] <: Edge[X]] extends GenericMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N](node_1: N, node_2: N): This[N]
}

trait PartialHyperEdgeMapper[+This <: Edge[_]] extends PartialMapper with HyperEdgeMapper { this: AnyHyperEdge[_] =>
  def map[N]: PartialFunction[Iterable[N], This]
}

trait PartialDiHyperEdgeMapper[+This <: Edge[_]] extends PartialMapper with DiHyperEdgeMapper {
  this: AnyDiHyperEdge[_] =>
  def map[N]: PartialFunction[(Iterable[N], Iterable[N]), This]
}

trait PartialEdgeMapper[+This <: Edge[_]] extends PartialMapper with EdgeMapper { this: AnyEdge[_] =>
  def map[N]: PartialFunction[(N, N), This]
}
