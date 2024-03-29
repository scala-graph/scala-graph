package scalax.collection.hyperedges.ordered

import scalax.collection.generic
import scalax.collection.generic.{AbstractDiHyperEdge, AbstractHyperEdge, Edge, MultiEdge}

package object multilabeled {
  type LHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractHyperEdge[X] with MultiEdge] =
    scalax.collection.hyperedges.multilabeled.LHyperEdgeInfixConstructor[N, L, CC]

  type LDiHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractDiHyperEdge[X] with MultiEdge] =
    scalax.collection.hyperedges.multilabeled.LDiHyperEdgeInfixConstructor[N, L, CC]

  type GenericHyperEdgeMapper[+CC[X] <: Edge[X]]   = generic.GenericHyperEdgeMapper[CC]
  type GenericDiHyperEdgeMapper[+CC[X] <: Edge[X]] = generic.GenericDiHyperEdgeMapper[CC]
}
