package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.generic.Edge
import scalax.collection.generic.GenericGraphCoreFactory

class MappingTypedHyperSpec
    extends Suites(
      new MappingTypedHyper[immutable.Graph](immutable.Graph),
      new MappingTypedHyper[mutable.Graph](mutable.Graph)
    )

private class MappingTypedHyper[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {}
