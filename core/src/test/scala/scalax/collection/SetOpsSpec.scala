package scalax.collection

import org.scalatest._
import org.scalatest.refspec.RefSpec

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import scalax.collection.visualization.Visualizer
import org.scalatest.matchers.should.Matchers

class SetOpsSpec
    extends Suites(
      new SetOps[immutable.Graph](immutable.Graph),
      new SetOps[mutable.Graph](mutable.Graph),
      new SetOpsImmutable,
      new SetOpsMutable
    )

protected trait SetOpExamples[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]] {

  protected def factory: GraphCoreCompanion[CC]

  protected val gEdges = Set(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  protected val hEdges = Set(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  protected def g = factory.from(gEdges)
  protected def h = factory.from(hEdges)

  protected object Expected {
    val g_union_h = factory.from(gEdges ++ hEdges)
    val g_diff_h =
      (g.nodes.toOuter -- h.nodes.toOuter) pipe { nDiff =>
        factory.from(nDiff, (gEdges ++ hEdges) filter { case n1 ~ n2 => nDiff(n1) && nDiff(n2) })
      }
  }
}

private class SetOps[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with SetOpExamples[CC]
    with Visualizer[CC] {

  def `concat ` : Unit = {
    // TODO
    factory(1 ~ 2) concat List(1 ~ 2): Graph[Int, UnDiEdge[Int]]
    factory(1 ~ 2) concat List(1 ~> 2): Graph[Int, AnyEdge[Int]]
    factory(1 ~ 2) ++ List(1 ~ 2): Graph[Int, UnDiEdge[Int]]

    factory(1 ~ 2) concat List("a" ~ "b"): Graph[Any, UnDiEdge[Any]]
    factory(1 ~ 2) concat (List('x'), List("a" ~ "b")): Graph[Any, UnDiEdge[Any]]

    factory(1 ~ 2) concat (List('x'), List('a' ~ 'b')): Graph[AnyVal, UnDiEdge[AnyVal]]
    factory(1 ~ 2) ++ (List('x'), List('a' ~ 'b')): Graph[AnyVal, UnDiEdge[AnyVal]]
  }

  def `union ` {
    g union h shouldEqual Expected.g_union_h
  }

  def `difference ` {
    g diff h shouldEqual Expected.g_diff_h
  }

  def `intersection ` {
    val expected = factory(3 ~ 5, 4)
    given(g intersect h)(_ shouldEqual expected)
    given(g & h)(_ shouldEqual expected)
  }
}

private class SetOpsImmutable extends RefSpec with Matchers with SetOpExamples[immutable.Graph] {
  protected val factory = immutable.Graph

}

private class SetOpsMutable extends RefSpec with Matchers with SetOpExamples[mutable.Graph] {
  protected val factory = mutable.Graph

  private val iH = immutable.Graph.from(hEdges)

  def `unionInPlace ` {
    (g |= h) shouldEqual Expected.g_union_h
    (g |= iH) shouldEqual Expected.g_union_h
  }

  def `--= ` {
    (g --= h) shouldEqual Expected.g_diff_h
    (g --= iH) shouldEqual Expected.g_diff_h
  }

  def `&= ` {
    val expected = factory(3 ~ 5, 4)

    (g &= h) shouldEqual expected
    (g &= iH) shouldEqual expected
  }
}
