package scalax.collection
package io.edge

object Types {
  type SeveralNodeIds   = Several[String]
  type OneOrMoreNodeIds = OneOrMore[String]
}
import Types._

sealed trait Parameters

case class EdgeParameters(n1: String, n2: String) extends Parameters

case class LEdgeParameters[L](n1: String, n2: String, label: L) extends Parameters

case class HyperEdgeParameters(nodeIds: SeveralNodeIds) extends Parameters

case class LHyperEdgeParameters[L](nodeIds: SeveralNodeIds, label: L) extends Parameters

case class DiHyperEdgeParameters(sources: OneOrMoreNodeIds, targets: OneOrMoreNodeIds) extends Parameters

case class LDiHyperEdgeParameters[L](sources: OneOrMoreNodeIds, targets: OneOrMoreNodeIds, label: L) extends Parameters
