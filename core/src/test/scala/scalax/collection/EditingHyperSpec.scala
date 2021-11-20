package scalax.collection
import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec
import scalax.collection.GraphEdge.{~~, ~~>, AnyHyperEdge, EdgeLike}
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion

/** Editing any kind of hypergraph with unlabeled edges including mixed and multigraphs.
  */
class EditingHyperSpec
    extends Suites(
      new EditingHyper[immutable.Graph](immutable.Graph),
      new EditingHyper[mutable.Graph](mutable.Graph),
      new EditingHyperImmutable,
      new EditingHyperMutable
    )

class EditingHyper[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers {

  object `hypergraph editing` {

    def `isHyper ` : Unit = {
      def test(g: CC[Int, AnyHyperEdge[Int]], expected: Boolean): Unit = g.isHyper should be(expected)

      import HyperEdgeImplicits._
      test(factory.from[Int, AnyHyperEdge](List(1 ~> 2, 1 ~~ 2 ~~ 3)), true)
      test(factory.from[Int, AnyHyperEdge](1 ~ 2 :: Nil), false)
      test(factory.from[Int, AnyHyperEdge](1 ~> 2 :: Nil), false)
    }
  }

  private val hDi = {
    import DiHyperEdgeImplicits._
    factory(1 ~~> List(1, 5), 1 ~~> List(2, 5), 1 ~~> List(3, 5), 1 ~~> List(4, 9))
  }

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
    import HyperEdgeImplicits._
    val hyper                      = 1 ~~ 2 ~~ 3
    val ~~(Seq(n1, n2, n3, _ @_*)) = hyper
    (n1 + n2 + n3) should be(6)
    // TODO (hyper match { case HyperEdge(n1 ~~ (n2, n3)            => n1 + n2 + n3 }) should be(6)
  }

  def `match dircted hyperedge`: Unit = {
    import DiHyperEdgeImplicits._
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
      import scalax.collection.GraphPredef.DiHyperEdgeImplicits._
      val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, 1 ~~> List(1, 2), 1 ~~> List(2, 3))
      val g                                         = Graph(oneOneTwo, oneTwoThree)
      val (n1, n2)                                  = (g get one, g get two)

      n2.diSuccessors shouldBe empty
      n1.diSuccessors should be(Set(two, three))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g subtractOne oneTwoThree // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(two))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g subtractOne two // Graph(one)
      n1.diSuccessors shouldBe empty
      (n1 ~>? n1) should be(None)

      g += oneOneTwo // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(2))
      (n1 ~>? n1) should be(Some(oneOneTwo))
    }
  }
}
