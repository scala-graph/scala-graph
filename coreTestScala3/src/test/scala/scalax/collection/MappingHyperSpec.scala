package scalax.collection

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.OneOrMore.{more, one}
import scalax.collection.OuterImplicits.*
import scalax.collection.hyperedges.*
import scalax.collection.immutable.Graph

class MappingHyperSpec extends RefSpec with Matchers {

  object `mapping a generic hypergraph you can` {
    val g = Graph(
      1 ~~ 2 ~~ 3,
      3 ~~ 4 ~~ 1
    )

    def `map by identity`: Unit =
      g.map(identity) shouldEqual g

    def `map nodes`: Unit =
      g.map(_.toString) shouldEqual Graph("1" ~~ "2" ~~ "3", "3" ~~ "4" ~~ "1")

    def `map both nodes and edges`: Unit =
      g.mapHyper(
        fNode = _.outer + 1,                                             // increment node values
        fHyperEdge = (newEnds: Several[Int]) => newEnds(0) ~~ newEnds(1) // add only the first two ends
      ) shouldEqual Graph(2 ~~ 3, 4 ~~ 5)
  }

  object `mapping a generic directed hypergraph you can` {
    val g = Graph(
      more(1, 2) ~~> one(3),
      one(3) ~~> more(4, 1)
    )

    def `map by identity`: Unit =
      g.map(identity) shouldEqual g

    def `map nodes`: Unit =
      g.map(_.toString) shouldEqual Graph(more("1", "2") ~~> one("3"), one("3") ~~> more("4", "1"))

    def `map both nodes and edges`: Unit =
      g.mapDiHyper(
        fNode = _.outer + 1, // increment node values
        fDiHyperEdge =
          // keep newSources as sources, add newSources to targets
          (newSources: OneOrMore[Int], newTargets: OneOrMore[Int]) => newSources ~~> (newTargets ++ newSources)
      ) shouldEqual Graph(
        more(2, 3) ~~> more(4, 2, 3),
        one(4) ~~> more(5, 2, 4)
      )
  }

  object `flat-mapping a generic hypergraph you can` {
    val g = Graph(
      1 ~~ 2 ~~ 3,
      3 ~~ 4 ~~ 1
    )

    def `map elements, change node type and edge cardinality`: Unit =
      g.flatMapHyper(
        fNode = (n: g.NodeT) =>
          /* nodes will be mapped to
             1 -> 2
             2 -> 2, -2
             3 -> 4
             4 -> 4, -4
           */
          if (n.outer % 2 == 0) n.toString :: -n.toString :: Nil
          else (n.outer + 1).toString :: Nil,

        // `fromUnsafe` is fine here because we know that `fNode` returns at least one mapped node
        fHyperEdge = (nn: Seq[String]) => HyperEdge.fromUnsafe(nn) :: Nil
      ) shouldEqual Graph(
        "-2" ~~ "2" ~~ "2" ~~ "4",
        "-4" ~~ "2" ~~ "4" ~~ "4"
      )

    def `change the graph structure`: Unit =
      g.flatMapHyper(
        fNode = (n: g.NodeT) => (n.outer + 1) :: Nil,
        fHyperEdge = (e: g.EdgeT, nn: Seq[Int]) =>
          nn match {
            case Seq(nn1, nn2, nn3) =>
              nn1 ~~ nn2 ~~ nn3 ~~ e.ends.iterator.map(_.degree).max ::
                nn1 ~~ nn2 ~~ -e.ends.size ::
                Nil
            case _ => Nil
          },
        fDiHyperEdge = None,
        fEdge = None
      ) shouldEqual Graph(
        2 ~~ 3 ~~ 4 ~~ 2,
        2 ~~ 3 ~~ -3,
        4 ~~ 5 ~~ 2 ~~ 2,
        4 ~~ 5 ~~ -3
      )
  }

  object `flat-mapping a generic directed hypergraph you can` {
    val g = Graph(
      more(1, 2) ~~> one(3),
      one(3) ~~> more(4, 1)
    )

    def `map elements, change node type and edge cardinality`: Unit =
      g.flatMapDiHyper(
        fNode = (n: g.NodeT) =>
          /* nodes will be mapped to
             1 -> 2
             2 -> 2, -2
             3 -> 4
             4 -> 4, -4
           */
          if (n.outer % 2 == 0) n.toString :: -n.toString :: Nil
          else (n.outer + 1).toString :: Nil,
        fDiHyperEdge =
          // `fromUnsafe` is fine here because above `fNode` returns at least one mapped node
          (newSources: Seq[String], newTargets: Seq[String]) => DiHyperEdge.fromUnsafe(newSources, newTargets) :: Nil
      ) shouldEqual Graph(
        more("-2", "2", "2") ~~> one("4"),
        one("4") ~~> more("4", "-4", "2")
      )

    def `change the graph structure`: Unit =
      g.flatMapDiHyper(
        fNode = (n: g.NodeT) => (n.outer + 1) :: Nil,
        fDiHyperEdge = (e: g.EdgeT, newSources: Seq[Int], newTargets: Seq[Int]) =>
          // `fromUnsafe` is fine here because above `fNode` always returns exactly one mapped node
          DiHyperEdge.fromUnsafe(newSources, newTargets ++ e.targets.iterator.map(_.outDegree)) ::
            DiHyperEdge.fromUnsafe(newTargets, newSources :+ e.sources.size) ::
            Nil,
        fEdge = None
      ) shouldEqual Graph(
        more(2, 3) ~~> more(4, 1),
        one(4) ~~> more(2, 3, 2),
        one(4) ~~> more(5, 2, 0, 1),
        more(5, 2) ~~> more(4, 1)
      )
  }
}
