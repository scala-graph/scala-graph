package scalax.collection

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalax.collection.generic._
import scalax.collection.edges._

protected trait WalkBehaviors {
  this: AnyFlatSpec with Matchers =>

  import UnDi_1._

  protected def walk(builder: => g.WalkBuilder): Unit = {
    it should "accept neighbors" in { builder add n(3) shouldBe true }
    it should "refuse non-neighbors" in { builder add n(4) shouldBe false }
    it should "accept outgoing edge" in { builder add (g get 1 ~ 2) shouldBe true }
    it should "refuse non-outgoing edge" in { builder add (g get 2 ~ 3) shouldBe false }
  }
}

class PathBuilderSpec extends AnyFlatSpec with WalkBehaviors with Matchers {

  import UnDi_1._

  def walkBuilder = g.newWalkBuilder(n(1))
  def pathBuilder = g.newPathBuilder(n(1))

  "A WalkBuilder" should behave like walk(walkBuilder)

  "A WalkBuilder" should "yield the expected Walk" in {
    val walk = (walkBuilder += n(3) += n(5) += n(1) += n(2)).result()
    walk.nodes.toList shouldBe List(1, 3, 5, 1, 2)
    walk.edges.toList shouldBe List(1 ~> 3, 3 ~ 5, 5 ~ 1, 1 ~ 2)
  }

  "A PathBuilder" should behave like walk(pathBuilder)

  "A PathBuilder" should "refuse duplicate nodes" in {
    (pathBuilder += n(2)) add n(1) shouldBe false
    (pathBuilder += n(2) += n(3)) add n(2) shouldBe false
  }

  "A PathBuilder" should "refuse duplicate edges" in {
    (pathBuilder += n(2) += n(3)) add e(2 ~ 3) shouldBe false
  }

  "PathBuilder result" should "discard a terminating edge" in {
    (pathBuilder += n(2) += e(2 ~ 3)).result().edges should have size 1
  }

  it should "yield the expected Path" in {
    val path = (pathBuilder += n(3) += n(5)).result()
    path.nodes.toList shouldBe List(1, 3, 5)
    path.edges.toList shouldBe List(1 ~> 3, 3 ~ 5)
  }
}

// The single graph used for this test
protected object UnDi_1 extends TGraph[Int, AnyEdge[Int], Graph](Graph.from(Data.elementsOfMixed_1))
