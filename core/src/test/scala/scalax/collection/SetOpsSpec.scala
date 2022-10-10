package scalax.collection

import scala.util.chaining._

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic.{Edge, GenericGraphCoreFactory}

import scalax.collection.visualization.Visualizer

class SetOpsSpec
    extends Suites(
      new SetOps[immutable.Graph](immutable.Graph),
      new SetOps[mutable.Graph](mutable.Graph),
      new SetOpsImmutable,
      new SetOpsMutable
    )

protected trait SetOpExamples[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] {

  protected def factory: GenericGraphCoreFactory[CC]

  protected val gEdges = Set(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  protected val hEdges = Set(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  protected def g = factory.from(gEdges)
  protected def h = factory.from(hEdges)

  protected object Expected {
    val g_union_h = factory.from(gEdges ++ hEdges)
    val g_diff_h =
      g.nodes.toOuter -- h.nodes.toOuter pipe { nDiff =>
        factory.from(nDiff, gEdges ++ hEdges filter { case n1 ~ n2 => nDiff(n1) && nDiff(n2) })
      }
  }
}

private class SetOps[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers
    with SetOpExamples[CC]
    with IntelliJ[CC]
    with Visualizer {

  def `concat ` : Unit = {
    factory(1 ~ 2).asAnyGraph concat List(1 ~ 2)
    factory(1 ~ 2).asAnyGraph concat List(1 ~> 2)
    factory(1 ~ 2).asAnyGraph ++ List(1 ~ 2)

    factory(1 ~ 2) concat List("a" ~ "b"): AnyGraph[Any, UnDiEdge[Any]]
    factory(1 ~ 2) concat (List('x'), List("a" ~ "b")): AnyGraph[Any, UnDiEdge[Any]]

    factory(1 ~ 2).asAnyGraph concat (List('x'), List('a' ~ 'b'))
    factory(1 ~ 2).asAnyGraph ++ (List('x'), List('a' ~ 'b'))
  }

  def `union ` : Unit =
    g union h shouldEqual Expected.g_union_h

  def `difference ` : Unit =
    g diff h shouldEqual Expected.g_diff_h

  def `intersection ` : Unit = {
    val expected = factory(3 ~ 5, 4)
    withGraph(g intersect h)(_ shouldEqual expected)
    withGraph(g & h)(_ shouldEqual expected)
  }
}

private class SetOpsImmutable extends RefSpec with Matchers with SetOpExamples[immutable.Graph] {
  protected val factory = immutable.Graph

}

private class SetOpsMutable extends RefSpec with Matchers with SetOpExamples[mutable.Graph] {
  protected val factory = mutable.Graph

  private val iH = immutable.Graph.from(hEdges)

  def `unionInPlace ` : Unit = {
    (g |= h) shouldEqual Expected.g_union_h
    (g |= iH) shouldEqual Expected.g_union_h
  }

  def `--= ` : Unit = {
    (g --= h) shouldEqual Expected.g_diff_h
    (g --= iH) shouldEqual Expected.g_diff_h
  }

  def `&= ` : Unit = {
    val expected = factory(3 ~ 5, 4)

    (g &= h) shouldEqual expected
    (g &= iH) shouldEqual expected
  }
}
