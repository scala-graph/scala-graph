package scalax.collection

import org.scalatest.refspec.RefSpec
import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites

import scalax.collection.OuterImplicits._
import scalax.collection.generic._
import scalax.collection.hyperedges._
import scalax.collection.generic.GraphCoreCompanion

import MappingHyperSpec._

class MappingHyperSpec
    extends Suites(
      new Common[immutable.Graph](immutable.Graph),
      new Common[mutable.Graph](mutable.Graph)
    )

object MappingHyperSpec {
  private class Common[CC[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
      val factory: GraphCoreCompanion[CC]
  ) extends RefSpec
      with Matchers {

    object `having a hypergraph with generic edges` {
      val g = Graph(1 ~~ 2 ~~ 3, 3 ~~ 4 ~~ 1)

      def `mapping it by identity yields the same hypergraph`: Unit = {
//        g.map(identity) shouldEqual g
      }
    }
  }
}
