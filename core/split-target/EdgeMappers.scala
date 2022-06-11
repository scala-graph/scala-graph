package scalax.collection.generic

sealed protected[collection] trait Mapper
sealed protected[collection] trait GenericMapper extends Mapper
sealed protected[collection] trait PartialMapper extends Mapper

sealed protected[collection] trait HyperEdgeMapper   extends Mapper
sealed protected[collection] trait DiHyperEdgeMapper extends Mapper
sealed protected[collection] trait EdgeMapper        extends Mapper

sealed trait GenericHyperEdgeMapper   extends GenericMapper with HyperEdgeMapper
sealed trait GenericDiHyperEdgeMapper extends GenericMapper with DiHyperEdgeMapper

trait GenericEdgeMapper[+N, +This[X] <: AnyEdge[X]] extends GenericMapper with EdgeMapper {
  def map[NN](node_1: NN, node_2: NN): This[NN]
}

trait PartialHyperEdgeMapper[+N, +This <: AnyHyperEdge[N]] extends PartialMapper with HyperEdgeMapper {
  def map[NN]: PartialFunction[Iterable[NN], This]
}
trait PartialDiHyperEdgeMapper[+N, +This <: AnyDiHyperEdge[N]] extends PartialMapper with DiHyperEdgeMapper {
  def map[NN]: PartialFunction[(Iterable[NN], Iterable[NN]), This]
}
trait PartialEdgeMapper[+N, +This <: AnyEdge[N]] extends PartialMapper with EdgeMapper {
  def map[NN]: PartialFunction[(NN, NN), This]
}
