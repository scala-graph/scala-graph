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

private class EditingHyper[CC[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  object `hypergraph editing` {

    def `create HyperEdge`: Unit = {
      "HyperEdge(List(1))" shouldNot compile
      "HyperEdge(List(1): _*)" shouldNot compile

      HyperEdge.from(List(1)) shouldBe None
      an[IllegalArgumentException] shouldBe thrownBy {
        HyperEdge.unsafeFrom(List(1))
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
      "DiHyperEdge(List(1))" shouldNot compile
      "DiHyperEdge(List(1): _*)" shouldNot compile

      DiHyperEdge.from(List(1), Nil) shouldBe None
      an[IllegalArgumentException] shouldBe thrownBy {
        HyperEdge.unsafeFrom(List(1))
      }

      val sources = List(1, 2)
      val targets = List(2, 3)
      val h       = DiHyperEdge(sources.head, sources.tail: _*)(targets.head, targets.tail: _*)

      DiHyperEdge.from(sources, targets) shouldEqual Some(h)
      sources ~~> targets shouldEqual h
      h.arity shouldBe sources.size + targets.size
      h.toString shouldBe "{1, 2} ~~> {2, 3}"

      1 ~~> targets shouldEqual DiHyperEdge(1)(targets.head, targets.tail: _*)
      sources ~~> 1 shouldEqual DiHyperEdge(sources.head, sources.tail: _*)(1)
      1 ~~> 1 shouldEqual DiHyperEdge(1)(1)

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

  val hDi = factory(1 ~~> List(1, 5), 1 ~~> List(2, 5), 1 ~~> List(3, 5), 1 ~~> List(4, 9))

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
    val hyper                      = 1 ~~ 2 ~~ 3
    val ~~(Seq(n1, n2, n3, _ @_*)) = hyper
    n1 + n2 + n3 should be(6)
    // TODO (hyper match { case HyperEdge(n1 ~~ (n2, n3)            => n1 + n2 + n3 }) should be(6)
  }

  def `match directed hyperedge`: Unit = {
    val count               = 3
    val sources             = List.tabulate(count - 1)(_ + 1)
    val target              = count
    val diHyper             = sources ~~> target
    val ~~>(Seq(s1, _*), _) = diHyper
    s1 should be(sources.head)
    val _ ~~> targets = diHyper
    targets.head should be(target)
  }
}

private class EditingHyperImmutable extends RefSpec with Matchers {
  // import immutable.Graph
}

private class EditingHyperMutable extends RefSpec with Matchers {
  import mutable.Graph

  object `mutable graphs with labeled edges` {
    def `'diSuccessors' when directed hypergraph`: Unit = {
      val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, 1 ~~> List(1, 2), 1 ~~> List(2, 3))
      val g                                         = Graph(oneOneTwo, oneTwoThree)
      val (n1, n2)                                  = (g get one, g get two)

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
