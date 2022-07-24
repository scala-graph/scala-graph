package scalax.collection.hyperedges.ordered

import scalax.collection.generic
import scalax.collection.generic.Edge

package object labeled {
  type GenericHyperEdgeMapper[+CC[X] <: Edge[X]]   = generic.GenericHyperEdgeMapper[CC]
  type GenericDiHyperEdgeMapper[+CC[X] <: Edge[X]] = generic.GenericDiHyperEdgeMapper[CC]
}
