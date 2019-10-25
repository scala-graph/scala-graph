package scalax.collection

import GraphEdge._, GraphPredef._

import org.scalatest._

// The single graph used for this test
object AGraph {
  import Data._
  object UnDi_1 extends TGraph[Int, UnDiEdge, Graph](Graph(elementsOfUnDi_1: _*))
}

// Common to WalkBuilder and PathBuilder
trait WalkBehaviors {
  this: FlatSpec with Matchers =>

  import AGraph.UnDi_1._

  def walk(builder: => g.WalkBuilder) {
    it should "accept neighbors" in { builder add n(3) should be(true) }
    it should "refuse non-neighbors" in { builder add n(4) should be(false) }
    it should "accept outgoing edge" in { builder add (g get 1 ~ 2) should be(true) }
    it should "refuse non-outgoing edge" in { builder add (g get 2 ~ 3) should be(false) }
  }
}

class TPathBuilderTest extends FlatSpec with WalkBehaviors with Matchers {

  import AGraph.UnDi_1._

  def walkBuilder = g.newWalkBuilder(n(1))
  def pathBuilder = g.newPathBuilder(n(1))

  "A WalkBuilder" should behave like walk(walkBuilder)

  "A WalkBuilder" should "yield the expected Walk" in {
    val walk = (walkBuilder += n(3) += n(5) += n(1) += n(2)).result
    walk.nodes.toList should be(List(1, 3, 5, 1, 2))
    walk.edges.toList should be(List(1 ~> 3, 3 ~ 5, 5 ~ 1, 1 ~ 2))
  }

  "A PathBuilder" should behave like walk(pathBuilder)

  "A PathBuilder" should "refuse duplicate nodes" in {
    (pathBuilder += n(2)) add n(1) should be(false)
    (pathBuilder += n(2) += n(3)) add n(2) should be(false)
  }

  "A PathBuilder" should "refuse duplicate edges" in {
    (pathBuilder += n(2) += n(3)) add e(2 ~ 3) should be(false)
  }

  "PathBuilder result" should "discard a terminating edge" in {
    (pathBuilder += n(2) += e(2 ~ 3)).result.edges should have size (1)
  }

  it should "yield the expected Path" in {
    val path = (pathBuilder += n(3) += n(5)).result
    path.nodes.toList should be(List(1, 3, 5))
    path.edges.toList should be(List(1 ~> 3, 3 ~ 5))
  }
}
