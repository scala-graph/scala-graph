package scalax.collection

import scala.collection.{SortedMap, SortedSet}

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.visualization.Visualizer

class DegreeSpec
    extends Suites(
      new Degree[immutable.Graph](immutable.Graph),
      new Degree[mutable.Graph](mutable.Graph)
    )

class Degree[CC[N, E <: Edge[N]] <: GraphLike[N, E, CC] with AnyGraph[N, E]](val factory: GenericGraphCoreFactory[CC])
    extends RefSpec
    with Matchers
    with Visualizer
    with IntelliJ[CC] {

  private val emptyG: AnyGraph[Int, DiEdge[Int]] = factory.empty[Int, DiEdge[Int]].asAnyGraph

  abstract class TestGraph[N, E <: Edge[N]](override val g: CC[N, E]) extends TGraph(g) {
    def degree(outer: N)                  = node(outer).degree
    val nodeDegrees: List[(g.NodeT, Int)] = g.nodes.iterator.map(n => (n, n.degree)).toList
    val degrees: List[Int]                = nodeDegrees map (_._2)
  }

  import Data._
  object UnDi_1 extends TestGraph[Int, AnyEdge[Int]](factory.from(elementsOfMixed_1)) {
    val expectedDegreeSeq         = Seq(4, 4, 3, 3, 2)
    val expectedDegreeSet         = SortedSet(4, 3, 2)
    val expectedDegreeNodeSeq     = Seq((4, node(4)), (4, node(3)), (3, node(5)), (3, node(1)), (2, node(2)))
    val expectedDegreeNodesMap    = SortedMap((4, Set(node(3), node(4))), (3, Set(node(1), node(5))), (2, Set(node(2))))
    val expectedDegreeCount       = SortedMap((4, 2), (3, 2), (2, 1))
    val expectedInDegreeNodeSeq   = Seq((4, node(3)), (3, node(5)), (2, node(4)), (2, node(2)), (2, node(1)))
    val expectedDegreeGT3NodesMap = SortedMap((4, Set(node(3), node(4))))
  }
  object UnDi_2 extends TestGraph[Int, AnyEdge[Int]](factory.from(elementsOfMixed_2)) {
    val expectedDegreeSeq      = Seq(5, 4, 3)
    val expectedDegreeSet      = SortedSet(5, 4, 3)
    val expectedDegreeNodeSeq  = Seq((5, node(2)), (4, node(1)), (3, node(3)))
    val expectedDegreeNodesMap = SortedMap((5, Set(node(2))), (4, Set(node(1))), (3, Set(node(3))))
    val expectedDegreeCount    = SortedMap((5, 1), (4, 1), (3, 1))
  }

  object `Degrees are calculated properly` {
    def `for nodes`: Unit = {
      {
        import UnDi_1._
        given(g.asAnyGraph) { _ =>
          degree(1) should be(3)
          degree(2) should be(2)
          degree(3) should be(4)
          degree(4) should be(4)
          degree(5) should be(3)
        }
      }
      {
        import UnDi_2._
        given(g.asAnyGraph) { _ =>
          degree(1) should be(4)
          degree(2) should be(5)
          degree(3) should be(3)
        }
      }
    }

    def `for total graph`: Unit = {
      emptyG.totalDegree shouldBe 0;
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.totalDegree shouldBe degrees.sum)
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.totalDegree shouldBe degrees.sum)
      }
    }
  }

  object `Degree statistics are calculated properly for` {
    def `minimum degree`: Unit = {
      emptyG.minDegree should be(0);
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.minDegree should be(degrees.min))
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.minDegree should be(degrees.min))
      }
    }

    def `maximum degree`: Unit = {
      emptyG.maxDegree should be(0);
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.maxDegree should be(degrees.max))
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.maxDegree should be(degrees.max))
      }
    }

    def `sequence of degrees`: Unit = {
      emptyG.degreeSeq should be(Seq.empty);
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.degreeSeq should be(expectedDegreeSeq))
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.degreeSeq should be(expectedDegreeSeq))
      }
    }

    def `set of degrees`: Unit = {
      emptyG.degreeSet should be(Set.empty);
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.degreeSet should be(expectedDegreeSet))
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.degreeSet should be(expectedDegreeSet))
      }
    }

    def `sequence of nodes sorted by degree`: Unit = {
      emptyG.degreeNodeSeq should be(Seq.empty);
      {
        import UnDi_1._
        given(g.asAnyGraph) { g =>
          val ord = new Ordering[g.DegreeNodeSeqEntry] {
            def compare(a: g.DegreeNodeSeqEntry, b: g.DegreeNodeSeqEntry) = {
              def sortKey(e: g.DegreeNodeSeqEntry) = 100 * e._1 + e._2

              sortKey(b) compare sortKey(a)
            }
          }

          val ds       = g.degreeNodeSeq
          val dsSorted = ds.toList sorted ord
          dsSorted should be(expectedDegreeNodeSeq)

          val ids       = g.degreeNodeSeq(g.InDegree)
          val idsSorted = ids.toList sorted ord
          idsSorted should be(expectedInDegreeNodeSeq)
        }
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.degreeNodeSeq should be(expectedDegreeNodeSeq))
      }
    }

    def `map of nodes by degree`: Unit = {
      emptyG.degreeNodesMap should be(Map.empty);
      {
        import UnDi_1._
        given(g.asAnyGraph) { g =>
          g.degreeNodesMap should be(expectedDegreeNodesMap)
          g.degreeNodesMap(degreeFilter = _ > 3) should be(expectedDegreeGT3NodesMap)
        }
      }
      {
        import UnDi_2._
        given(g.asAnyGraph) { g =>
          g.degreeNodesMap should be(expectedDegreeNodesMap)
        }
      }
    }

    def `map of degree by node`: Unit = {
      emptyG.degreeCount should be(Map.empty);
      {
        import UnDi_1._
        given(g.asAnyGraph)(_.degreeCount should be(expectedDegreeCount))
      }
      {
        import UnDi_2._
        given(g.asAnyGraph)(_.degreeCount should be(expectedDegreeCount))
      }
    }
  }
}
