package scalax.collection

import language.{higherKinds, postfixOps}
import collection.{SortedSet, SortedMap}

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TDegreeRootTest
  extends Suites(
      new TDegree[immutable.Graph](immutable.Graph),
      new TDegree[  mutable.Graph](  mutable.Graph))

/**	This class contains tests for degree operations.
 */
class TDegree[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC])
	extends	Spec
	with	  ShouldMatchers
{
  val emptyG = factory.empty[Int,DiEdge]
  abstract class TGraphDegree[N, E[X] <: EdgeLikeIn[X]](override val g: Graph[N,E])
    extends TGraph[N, E](g)
  {
    def degree(outer: N) = node(outer) degree
    val nodeDegrees: List[(g.NodeT, Int)] = g.nodes.toList map (n => (n, n.degree))
    val degrees:     List[Int]            = nodeDegrees map (_._2)
  }
  import Data._
  object UnDi_1 extends TGraphDegree[Int, UnDiEdge](
    factory(elementsOfUnDi_1: _*))
  {
    val expectedDegreeSeq = Seq(4,4,3,3,2)
    val expectedDegreeSet = SortedSet(4,3,2)
    val expectedDegreeNodeSeq = Seq((4, node(4)),
                                    (4, node(3)),
                                    (3, node(5)),
                                    (3, node(1)),
                                    (2, node(2)))
    val expectedDegreeNodesMap = SortedMap((4, Set(node(3),node(4))),
                                           (3, Set(node(1),node(5))),
                                           (2, Set(node(2))))
    val expectedDegreeCount = SortedMap((4, 2),
                                        (3, 2),
                                        (2, 1))
    val expectedInDegreeNodeSeq = Seq((4, node(3)),
                                      (3, node(5)),
                                      (2, node(4)),
                                      (2, node(2)),
                                      (2, node(1)))
    val expectedDegreeGT3NodesMap = SortedMap((4, Set(node(3),node(4))))
  }
  object UnDi_2 extends TGraphDegree[Int, UnDiEdge](
    factory(elementsOfUnDi_2: _*))
  {
    val expectedDegreeSeq = Seq(5,4,3)
    val expectedDegreeSet = SortedSet(5,4,3)
    val expectedDegreeNodeSeq = Seq((5, node(2)),
                                    (4, node(1)),
                                    (3, node(3)))
    val expectedDegreeNodesMap = SortedMap((5, Set(node(2))),
                                           (4, Set(node(1))),
                                           (3, Set(node(3))))
    val expectedDegreeCount = SortedMap((5, 1),
                                        (4, 1),
                                        (3, 1))
  }
  def test_nodeDegrees {
    { import UnDi_1._
      degree(1) should be (3)
      degree(2) should be (2)
      degree(3) should be (4)
      degree(4) should be (4)
      degree(5) should be (3)
    }
    { import UnDi_2._
      degree(1) should be (4)
      degree(2) should be (5)
      degree(3) should be (3)
    }
  }
  def test_totalDegree {
    emptyG .totalDegree should be (0);
    { import UnDi_1._
      g.totalDegree should be (degrees sum)
    }
    { import UnDi_2._
      g.totalDegree should be (degrees sum)
    }
  }
  def test_minDegree {
    emptyG .minDegree should be (0);
    { import UnDi_1._
      g.minDegree should be (degrees min)
    }
    { import UnDi_2._
      g.minDegree should be (degrees min)
    }
  }
  def test_maxDegree {
    emptyG .maxDegree should be (0);
    { import UnDi_1._
      g.maxDegree should be (degrees max)
    }
    { import UnDi_2._
      g.maxDegree should be (degrees max)
    }
  }
  def test_degreeSeq {
    emptyG.degreeSeq should be (Seq.empty);
    { import UnDi_1._
      g.degreeSeq should be (expectedDegreeSeq)
    }
    { import UnDi_2._
      g.degreeSeq should be (expectedDegreeSeq)
    }
  }
  def test_degreeSet {
    emptyG.degreeSet should be (Set.empty);
    { import UnDi_1._
      g.degreeSet should be (expectedDegreeSet)
    }
    { import UnDi_2._
      g.degreeSet should be (expectedDegreeSet)
    }
  }
  def test_degreeNodeSeq {
    emptyG.degreeNodeSeq should be (Seq.empty);
    { import UnDi_1._
      val ord = new Ordering[g.DegreeNodeSeqEntry] {
        def compare(a: g.DegreeNodeSeqEntry, b: g.DegreeNodeSeqEntry) = {
          def sortKey(e: g.DegreeNodeSeqEntry) = 100 * e._1 + e._2
          sortKey(b) compare sortKey(a)
        }
      }

      val ds = g.degreeNodeSeq
      val dsSorted = ds.toList sorted ord
      dsSorted should be (expectedDegreeNodeSeq)

      val ids = g.degreeNodeSeq(g.InDegree)
      val idsSorted = ids.toList sorted ord
      idsSorted should be (expectedInDegreeNodeSeq)
    }
    { import UnDi_2._
      g.degreeNodeSeq should be (expectedDegreeNodeSeq)
    }
  }
  def test_degreeNodesMap {
    emptyG.degreeNodesMap should be (Map.empty);
    { import UnDi_1._
      g.degreeNodesMap should be (expectedDegreeNodesMap)
      g.degreeNodesMap(degreeFilter = _ > 3) should be (expectedDegreeGT3NodesMap)
    }
    { import UnDi_2._
      g.degreeNodesMap should be (expectedDegreeNodesMap)
    }
  }
  def test_degreeCount {
    emptyG.degreeCount should be (Map.empty);
    { import UnDi_1._
      g.degreeCount should be (expectedDegreeCount)
    }
    { import UnDi_2._
      g.degreeCount should be (expectedDegreeCount)
    }
  }
}