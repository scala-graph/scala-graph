package scalax.collection

import org.scalatest.{Matchers, Suites}
import org.scalatest.refspec.RefSpec
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.generic.GraphCoreCompanion

class MappingTypedHyperSpec
    extends Suites(
      new MappingTypedHyper[immutable.Graph](immutable.Graph),
      new MappingTypedHyper[mutable.Graph](mutable.Graph)
    )

private class MappingTypedHyper[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {}
