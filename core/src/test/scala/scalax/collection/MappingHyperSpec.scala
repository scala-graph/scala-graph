package scalax.collection

import org.scalatest.refspec.RefSpec
import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites

import scalax.collection.OuterImplicits._
import scalax.collection.generic._
import scalax.collection.hyperedges._
import scalax.collection.generic.GenericGraphCoreFactory

import MappingHyperSpec._

class MappingHyperSpec
    extends Suites(
      new Common[immutable.Graph](immutable.Graph),
      new Common[mutable.Graph](mutable.Graph)
    )

object MappingHyperSpec {
  private class Common[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
      val factory: GenericGraphCoreFactory[CC]
  ) extends RefSpec
      with Matchers {

    object `generic hypergraph` {
      val g = factory(1 ~~ 2 ~~ 3, 3 ~~ 4 ~~ 1)

      def `map by identity`: Unit =
        g.map(identity) shouldEqual g

      def `map nodes`: Unit =
        g.map(_.toString) shouldEqual factory("1" ~~ "2" ~~ "3", "3" ~~ "4" ~~ "1")

      def `map both nodes and edges`: Unit =
        // increment node values and, mapping edges, add only the first two ends
        g.mapHyper(
          fNode = _.outer + 1,
          fHyperEdge = (newEnds: Several[Int]) => newEnds(0) ~~ newEnds(1)
        ) shouldEqual factory(2 ~~ 3, 4 ~~ 5)
    }

    object `generic directed hypergraph` {
      val g = factory(Several(1, 2) ~~> One(3), One(3) ~~> Several(4, 1))

      def `map by identity`: Unit =
        g.map(identity) shouldEqual g

      def `map nodes`: Unit =
        g.map(_.toString) shouldEqual factory(Several("1", "2") ~~> One("3"), One("3") ~~> Several("4", "1"))

      def `map both nodes and edges`: Unit =
        // increment node values and, mapping edges, add sources to targets ends
        g.mapDiHyper(
          fNode = _.outer + 1,
          fDiHyperEdge = (newSources: OneOrMore[Int], newTargets: OneOrMore[Int]) =>
            newSources ~~> Several.fromUnsafe(newTargets ++ newSources)
        ) shouldEqual factory(Several(2, 3) ~~> Several(4, 2, 3), One(4) ~~> Several(5, 2, 4))
    }
  }
}
