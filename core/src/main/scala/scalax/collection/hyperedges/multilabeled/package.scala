package scalax.collection.hyperedges

import scalax.collection.generic
import scalax.collection.generic.Edge

package object multilabeled {
  type GenericHyperEdgeMapper[+CC[X] <: Edge[X]]   = generic.GenericHyperEdgeMapper[CC]
  type GenericDiHyperEdgeMapper[+CC[X] <: Edge[X]] = generic.GenericDiHyperEdgeMapper[CC]
}
