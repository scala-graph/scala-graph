package scalax.collection

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.hyperedges._
import scalax.collection.generic._

/** Editing any kind of hypergraph with unlabeled edges including mixed and multigraphs.
  */
class EditingHyperSpec
    extends Suites(
      new EditingHyper[immutable.Graph](immutable.Graph),
      new EditingHyper[mutable.Graph](mutable.Graph),
      new EditingHyperImmutable,
      new EditingHyperMutable
    )

private class EditingHyper[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  object `hypergraph editing` {

    def `create HyperEdge`: Unit = {
      "HyperEdge(List(1))" shouldNot compile
      "HyperEdge(List(1): _*)" shouldNot compile

      HyperEdge.from(List(1)) shouldBe None
      an[IllegalArgumentException] shouldBe thrownBy {
        HyperEdge.fromUnsafe(List(1))
      }

      val h = HyperEdge(1, 2, 3)
      1 ~~ 2 ~~ 3 shouldEqual h
      h.arity shouldBe 3
      h.toString shouldBe "1 ~~ 2 ~~ 3"

      val g = factory[Int, AnyHyperEdge](1, h, 1 ~ 2)
      g.nodes should have size 3
      g.edges should have size 2
      g.elementCount should be(5)
      g.contains(h) should be(true)
    }

    def `create DiHyperEdge`: Unit = {
      "DiHyperEdge(List(1), List(1))" shouldNot compile
      "DiHyperEdge(List(1): _*)()" shouldNot compile

      DiHyperEdge.from(List(1), Nil) shouldBe None
      an[IllegalArgumentException] shouldBe thrownBy {
        DiHyperEdge.unsafeFrom(List(1), Nil)
      }

      val sources = Several(1, 2)
      val targets = Several(2, 3)
      val h       = DiHyperEdge(sources, targets)

      DiHyperEdge.from(sources, targets) shouldEqual Some(h)
      sources ~~> targets shouldEqual h
      h.arity shouldBe sources.size + targets.size
      h.toString shouldBe "{1, 2} ~~> {2, 3}"

      one ~~> targets shouldEqual DiHyperEdge(one, targets)
      sources ~~> one shouldEqual DiHyperEdge(sources, one)
      one ~~> one shouldEqual DiHyperEdge(1)(1)

      val g = factory[Int, AnyHyperEdge](1, h, 1 ~ 2)
      g.nodes should have size 3
      g.edges should have size 2
      g.elementCount shouldBe 5
      g.contains(h) shouldBe true
    }

    def `isHyper ` : Unit = {
      def test(g: CC[Int, AnyHyperEdge[Int]], expected: Boolean): Unit = g.isHyper should be(expected)

      test(factory.from[Int, AnyHyperEdge](List(1 ~> 2, 1 ~~ 2 ~~ 3)), true)
      test(factory.from[Int, AnyHyperEdge](1 ~ 2 :: Nil), false)
      test(factory.from[Int, AnyHyperEdge](1 ~> 2 :: Nil), false)
    }
  }

  val one = One(1)
  val hDi = factory(
    one ~~> Several(1, 5),
    one ~~> Several(2, 5),
    one ~~> Several(3, 5),
    one ~~> Several(4, 9)
  )

  object `diSuccessors ` {
    def `for DiHyper`: Unit = {
      (hDi get 1).diSuccessors shouldEqual Set(2, 3, 4, 5, 9)
      (hDi get 2).diSuccessors shouldEqual Set.empty
      (hDi get 5).diSuccessors shouldEqual Set.empty
    }
  }

  object `diPredecessors ` {
    def `for DiHyper`: Unit = {
      (hDi get 1).diPredecessors should be(Set.empty)
      (hDi get 2).diPredecessors should be(Set(1))
      (hDi get 5).diPredecessors should be(Set(1))
    }
  }

  object `neighbors ` {
    def `for DiHyper`: Unit = {
      (hDi get 1).neighbors should be(Set(2, 3, 4, 5, 9))
      (hDi get 2).neighbors should be(Set(1, 5))
      (hDi get 5).neighbors should be(Set(1, 2, 3))
    }
  }

  def `match hyperedge`: Unit = {
    val HyperEdge(Several(Seq(n1, n2, n3))) = 1 ~~ 2 ~~ 3
    n1 + n2 + n3 shouldBe 6
  }

  def `match directed hyperedge`: Unit = {
    val count   = 3
    val sources = Several.fromUnsafe(List.tabulate(count - 1)(_ + 1))
    val target  = One(count)
    val diHyper = sources ~~> target

    val OneOrMore(Seq(s1, _)) ~~> (t @ One(c)) = diHyper
    s1 shouldBe sources.head
    c shouldBe count
    t shouldBe target
  }
}

private class EditingHyperImmutable extends RefSpec with Matchers {
  // import immutable.Graph
}

private class EditingHyperMutable extends RefSpec with Matchers {
  import mutable.Graph

  object `mutable graphs with labeled edges` {
    def `'diSuccessors' when directed hypergraph`: Unit = {
      val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, One(1) ~~> Several(1, 2), One(1) ~~> Several(2, 3))

      val g        = Graph(oneOneTwo, oneTwoThree)
      val (n1, n2) = (g get one, g get two)

      n2.diSuccessors shouldBe empty
      n1.diSuccessors should be(Set(two, three))
      n1 findOutgoingTo n1 should be(Some(oneOneTwo))

      g subtractOne oneTwoThree // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(two))
      n1 findOutgoingTo n1 should be(Some(oneOneTwo))

      g subtractOne two // Graph(one)
      n1.diSuccessors shouldBe empty
      n1 findOutgoingTo n1 should be(None)

      g += oneOneTwo // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(2))
      n1 findOutgoingTo n1 should be(Some(oneOneTwo))
    }
  }
}
