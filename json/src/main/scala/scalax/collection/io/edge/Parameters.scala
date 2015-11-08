package scalax.collection.io.edge

import scalax.collection.GraphEdge.CollectionKind

object Types {
  type EdgeNodeIds      = (String,String)
  type HyperEdgeNodeIds = List[String]
}
import Types._

sealed abstract class Parameters(nodeIds: Product)
class EdgeParameters(val n1: String, val n2: String) extends Parameters((n1, n2))
object EdgeParameters {
  def unapply(p: EdgeParameters): Option[EdgeNodeIds] = Some((p.n1, p.n2))
}

class WEdgeParameters(val n1:     String,
                      val n2:     String,
                      val weight: Long) extends Parameters((n1, n2))
object WEdgeParameters {
  def unapply(p: WEdgeParameters): Option[(EdgeNodeIds,Long)] = Some((p.n1, p.n2), p.weight)
}

class LEdgeParameters[L](val n1:    String,
                         val n2:    String,
                         val label: L) extends Parameters((n1, n2))
object LEdgeParameters {
  def unapply[L](p: LEdgeParameters[L]): Option[(EdgeNodeIds,L)] = Some((p.n1, p.n2), p.label)
}

class WLEdgeParameters[L](val n1:     String,
                          val n2:     String,
                          val weight: Long,
                          val label:  L) extends Parameters((n1, n2))
object WLEdgeParameters {
  def unapply[L](p: WLEdgeParameters[L]): Option[(EdgeNodeIds,Long,L)] =
    Some((p.n1, p.n2), p.weight, p.label)
}
class CEdgeParameters[P <: Product](val n1: String, val n2: String, val attributes: P)
  extends Parameters((n1, n2))
object CEdgeParameters {
  def unapply[A <: Product](p: CEdgeParameters[A]): Option[(EdgeNodeIds,A)] =
    Some((p.n1, p.n2), p.attributes)
}

class HyperEdgeParameters(val nodeIds: HyperEdgeNodeIds, val endpointsKind: String)
    extends Parameters(nodeIds)
object HyperEdgeParameters {
  def unapply(p: HyperEdgeParameters): Option[(HyperEdgeNodeIds,String)] =
    Some((p.nodeIds, p.endpointsKind.toString))
}
class WHyperEdgeParameters(val nodeIds: HyperEdgeNodeIds,
                           val endpointsKind: String,
                           val weight:  Long) extends Parameters(nodeIds)
object WHyperEdgeParameters {
  def unapply(p: WHyperEdgeParameters): Option[(HyperEdgeNodeIds,String,Long)] =
    Some(p.nodeIds, p.endpointsKind, p.weight)
}
class LHyperEdgeParameters[L](val nodeIds: HyperEdgeNodeIds,
                              val endpointsKind: String,
                              val label:   L) extends Parameters(nodeIds)
object LHyperEdgeParameters {
  def unapply[L](p: LHyperEdgeParameters[L]): Option[(HyperEdgeNodeIds,String,L)] =
    Some(p.nodeIds, p.endpointsKind.toString, p.label)
}
class WLHyperEdgeParameters[L](val nodeIds: HyperEdgeNodeIds,
                               val endpointsKind: String,
                               val weight:  Long,
                               val label:   L) extends Parameters(nodeIds)
object WLHyperEdgeParameters {
  def unapply[L](p: WLHyperEdgeParameters[L]): Option[(HyperEdgeNodeIds,String,Long,L)] =
    Some(p.nodeIds, p.endpointsKind.toString, p.weight, p.label)
}
class CHyperEdgeParameters[P <: Product](val nodeIds: HyperEdgeNodeIds,
                                         val endpointsKind: String,
                                         val attributes: P) extends Parameters(nodeIds)
object CHyperEdgeParameters {
  def unapply[A <: Product](p: CHyperEdgeParameters[A]): Option[(HyperEdgeNodeIds,String,A)] =
    Some((p.nodeIds), p.endpointsKind.toString, p.attributes)
}
