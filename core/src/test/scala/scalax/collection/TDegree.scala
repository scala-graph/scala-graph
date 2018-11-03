package scalax.collection

import language.{higherKinds, postfixOps}
import collection.{SortedMap, SortedSet}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TDegreeRootTest
    extends Suites(new TDegree[immutable.Graph](immutable.Graph), new TDegree[mutable.Graph](mutable.Graph))

class TDegree[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  val emptyG = factory.empty[Int, DiEdge]
  abstract class TGraphDegree[N, E[X] <: EdgeLikeIn[X]](override val g: CC[N, E]) extends TGraph(g) {
    def degree(outer: N)                  = node(outer) degree
    val nodeDegrees: List[(g.NodeT, Int)] = g.nodes.toList map (n => (n, n.degree))
    val degrees: List[Int]                = nodeDegrees map (_._2)
  }
  import Data._
  object UnDi_1 extends TGraphDegree[Int, UnDiEdge](factory(elementsOfUnDi_1: _*)) {
    val expectedDegreeSeq         = Seq(4, 4, 3, 3, 2)
    val expectedDegreeSet         = SortedSet(4, 3, 2)
    val expectedDegreeNodeSeq     = Seq((4, node(4)), (4, node(3)), (3, node(5)), (3, node(1)), (2, node(2)))
    val expectedDegreeNodesMap    = SortedMap((4, Set(node(3), node(4))), (3, Set(node(1), node(5))), (2, Set(node(2))))
    val expectedDegreeCount       = SortedMap((4, 2), (3, 2), (2, 1))
    val expectedInDegreeNodeSeq   = Seq((4, node(3)), (3, node(5)), (2, node(4)), (2, node(2)), (2, node(1)))
    val expectedDegreeGT3NodesMap = SortedMap((4, Set(node(3), node(4))))
  }
  object UnDi_2 extends TGraphDegree[Int, UnDiEdge](factory(elementsOfUnDi_2: _*)) {
    val expectedDegreeSeq      = Seq(5, 4, 3)
    val expectedDegreeSet      = SortedSet(5, 4, 3)
    val expectedDegreeNodeSeq  = Seq((5, node(2)), (4, node(1)), (3, node(3)))
    val expectedDegreeNodesMap = SortedMap((5, Set(node(2))), (4, Set(node(1))), (3, Set(node(3))))
    val expectedDegreeCount    = SortedMap((5, 1), (4, 1), (3, 1))
  }

  object `Degrees are calculated properly` {
    def `for nodes` {
      {
        import UnDi_1._
        given(g) { _ =>
          degree(1) should be(3)
          degree(2) should be(2)
          degree(3) should be(4)
          degree(4) should be(4)
          degree(5) should be(3)
        }
      }
      {
        import UnDi_2._
        given(g) { _ =>
          degree(1) should be(4)
          degree(2) should be(5)
          degree(3) should be(3)
        }
      }
    }
    def `for total graph` {
      emptyG.totalDegree should be(0);
      {
        import UnDi_1._
        given(g) { _.totalDegree should be(degrees sum) }
      }
      {
        import UnDi_2._
        given(g) { _.totalDegree should be(degrees sum) }
      }
    }
  }

  object `Degree statistics are calculated properly for` {
    def `minimum degree` {
      emptyG.minDegree should be(0);
      {
        import UnDi_1._
        given(g) { _.minDegree should be(degrees min) }
      }
      {
        import UnDi_2._
        given(g) { _.minDegree should be(degrees min) }
      }
    }
    def `maximum degree` {
      emptyG.maxDegree should be(0);
      {
        import UnDi_1._
        given(g) { _.maxDegree should be(degrees max) }
      }
      {
        import UnDi_2._
        given(g) { _.maxDegree should be(degrees max) }
      }
    }
    def `sequence of degrees` {
      emptyG.degreeSeq should be(Seq.empty);
      {
        import UnDi_1._
        given(g) { _.degreeSeq should be(expectedDegreeSeq) }
      }
      {
        import UnDi_2._
        given(g) { _.degreeSeq should be(expectedDegreeSeq) }
      }
    }
    def `set of degrees` {
      emptyG.degreeSet should be(Set.empty);
      {
        import UnDi_1._
        given(g) { _.degreeSet should be(expectedDegreeSet) }
      }
      {
        import UnDi_2._
        given(g) { _.degreeSet should be(expectedDegreeSet) }
      }
    }
    def `sequence of nodes sorted by degree` {
      emptyG.degreeNodeSeq should be(Seq.empty);
      {
        import UnDi_1._
        given(g) { g =>
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
        given(g) { _.degreeNodeSeq should be(expectedDegreeNodeSeq) }
      }
    }
    def `map of nodes by degree` {
      emptyG.degreeNodesMap should be(Map.empty);
      {
        import UnDi_1._
        given(g) { g =>
          g.degreeNodesMap should be(expectedDegreeNodesMap)
          g.degreeNodesMap(degreeFilter = _ > 3) should be(expectedDegreeGT3NodesMap)
        }
      }
      {
        import UnDi_2._
        given(g) { g =>
          g.degreeNodesMap should be(expectedDegreeNodesMap)
        }
      }
    }
    def `map of degree by node` {
      emptyG.degreeCount should be(Map.empty);
      {
        import UnDi_1._
        given(g) { _.degreeCount should be(expectedDegreeCount) }
      }
      {
        import UnDi_2._
        given(g) { _.degreeCount should be(expectedDegreeCount) }
      }
    }
  }
}
