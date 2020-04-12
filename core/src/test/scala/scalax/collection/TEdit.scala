package scalax.collection

import org.scalatest._
import org.scalatest.refspec.RefSpec

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCompanion

/** This wrapper trait enables to transparently pass `GraphCompanion` objects with non-default configuration parameters to tests.
  */
trait ConfigWrapper[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]] {
  val companion: GraphCompanion[CC]
  implicit val config: companion.Config

  def empty[N, E <: EdgeLike[N]](implicit config: companion.Config): CC[N, E] = companion.empty[N, E]

  def apply[N, E[X] <: EdgeLike[X]](elems: OuterElem[N, E[N]]*)(implicit config: companion.Config) =
    companion(elems: _*)

  def from[N, E <: EdgeLike[N]](nodes: collection.Iterable[N], edges: collection.Iterable[E])(
      implicit config: companion.Config) =
    companion.from[N, E](nodes, edges)

  def from[N, E[X] <: EdgeLike[X]](edges: collection.Iterable[E[N]]) = companion.from[N, E](edges)
}

class TEditRootTest
    extends Suites(
      new TEdit[Graph](new ConfigWrapper[Graph] {
        val companion = Graph
        val config    = Graph.defaultConfig
      }),
      new TEdit[immutable.Graph](new ConfigWrapper[immutable.Graph] {
        val companion = immutable.Graph
        val config    = immutable.Graph.defaultConfig
      }),
      new TEdit[mutable.Graph](new ConfigWrapper[mutable.Graph] {
        val companion = mutable.Graph
        val config    = mutable.Graph.defaultConfig
      }),
      new TEditImmutable,
      new TEditMutable
    )

class TEditImmutable extends RefSpec with Matchers {
  private val Graph = immutable.Graph

  Graph.from(1 ~ 2 :: Nil)

  object `graphs ` {
    def `are immutable by default` {
      val g = Graph[Nothing, Nothing]()
      g shouldBe a[immutable.Graph[_, Nothing]]
    }

    def `+ Int` {
      val g = Graph(1, 2 ~ 3)
      g + 1 should be(g)
      g + 0 should be(Graph(0, 1, 2, 3, 2 ~ 3))
      g + 0 ~ 1 should be(Graph(0, 1, 2, 3, 0 ~ 1, 2 ~ 3))
    }

    val gString_A = Graph("A")

    def `++ ` {
      val g = gString_A + "B" + "C"
      g.elementCount should be(3)
      ('A' to 'C') map (_.toString) foreach (g.contains(_) should be(true))

      val (gBefore, gAfter) = (Graph(1, 2 ~ 3), Graph(0, 1 ~ 2, 2 ~ 3))
      gBefore ++ (nodes = List(0), edges = List(1 ~ 2, 2 ~ 3)) should equal(gAfter)
      gBefore ++ Graph(0, 1 ~ 2) should equal(gAfter)
      gBefore ++ Graph[Int, UnDiEdge](0) ++ Graph(1 ~ 2) should equal(gAfter)
    }
    def `- ` {
      var g = gString_A - "B"
      g.order should be(1)

      g = gString_A - "A"
      g.contains("A") should be(false) //gMinus shouldNot contain ("A")
      g should have size 0
      g shouldBe empty

      val h = Graph(1, 2, 2 ~ 3)
      h - 0 should be(h)
      h - 1 should be(Graph(2, 2 ~ 3))
      h - 2 should be(Graph(1, 3))
    }

    def `-- ` {
      val g = Graph(1, 2 ~ 3, 3 ~ 4)
      g -- (nodes = List(2), edges = List(3 ~ 3)) should be(Graph(1, 3 ~ 4))
      g -- (nodes = List(2), edges = List(3 ~ 4)) should be(Graph(1, 3, 4))
    }

    def `+ String ` {
      val g = gString_A + "B"
      g.elementCount should be(2)
      g.contains("A") should be(true)
      g.contains("B") should be(true) //g should contain ("B")

      val hString_A = Graph[String, UnDiEdge]("A")
      val h         = hString_A + ("A" ~ "C")
      h.nodes should have size 2
      h.edges should have size 1
      h.elementCount should be(3)
    }
  }
}

class TEditMutable extends RefSpec with Matchers {
  private val Graph = mutable.Graph

  object `mutable graphs` {
    def `serve += properly` {
      val g = Graph[Int, Nothing](1, 3)
      g += 2
      g.order should be(3)
      for (i <- 1 to 3)
        g.contains(i) should be(true) //g should contain (i)
    }

    def `serve -= properly` {
      val g = Graph(1, 2, 2 ~ 3, 4)
      g remove 1 should be(true)
      g should be(Graph(2 ~ 3, 4))
      g remove 5 should be(false)
      (g -= 2) should be(Graph(3, 4))
      g.clear()
      g should be('empty)
    }

    def `+ String ` {
      val g = Graph("A") += "B"
      g.elementCount should be(2)
      g.contains("A") should be(true)
      g.contains("B") should be(true) //g should contain ("B")

      val hString_A = Graph[String, UnDiEdge]("A")
      val h         = hString_A += ("A" ~ "C")
      h.nodes should have size 2
      h.edges should have size 1
      h.elementCount should be(3)
    }

    def `serve -= properly (2)` {
      val g = Graph(1 ~ 2, 2 ~ 3)
      (g -= 2) should be(Graph(1, 3))
      g.size should be(0)
    }

    def `serve 'directed' properly` {
      val (di, unDi)                        = (1 ~> 2, 2 ~ 3)
      val g                                 = Graph[Int, AnyEdge](unDi)
      def directed(expected: Boolean): Unit = g.isDirected should be(expected)

      directed(false)
      g.clear(); directed(false)
      g += di; directed(true)
      g += unDi; directed(false)
      g.clear(); directed(false)
    }

    def `serve 'diSuccessors' when directed` {
      val (one, two, oneOne, oneTwo) = (1, 2, 1 ~> 1, 1 ~> 2)
      val g                          = Graph(oneOne, oneTwo, one ~> 3, one ~> 4)
      val (n1, n2)                   = (g get one, g get two)
      val e11                        = g get oneOne

      g -= 1 ~> 4 // Graph(oneOne, oneTwo, one~>3)
      n2.diSuccessors shouldBe empty
      n1.diSuccessors shouldBe (Set(two, 3))
      (n1 ~>? n1) should be(Some(e11))

      g -= oneTwo // Graph(oneOne, one~>3)
      n1.diSuccessors should be(Set(3))
      (n1 ~>? n1) should be(Some(e11))

      g -= oneOne // Graph(one~>3)
      n1.diSuccessors should be(Set(3))
      (n1 ~>? n1) should be(None)

      g ++= (edges = List(oneOne, oneTwo)) // Graph(oneOne, oneTwo, one~>3)
      n1.diSuccessors should be(Set(two, 3))
      (n1 ~>? n1) should be(Some(e11))
    }

    def `'diSuccessors' when directed hypergraph` {
      import DiHyperEdgeImplicits._
      val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, 1 ~~> List(1, 2), 1 ~~> List(2, 3))
      val g                                         = Graph(oneOneTwo, oneTwoThree)
      val (n1, n2)                                  = (g get one, g get two)
      val e112                                      = g get oneOneTwo

      n2.diSuccessors shouldBe empty
      n1.diSuccessors should be(Set(two, three))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g -= oneTwoThree // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(two))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g -= two // Graph(one)
      n1.diSuccessors shouldBe empty
      (n1 ~>? n1) should be(None)

      g += oneOneTwo // Graph(oneOneTwo)
      n1.diSuccessors should be(Set(2))
      (n1 ~>? n1) should be(Some(oneOneTwo))
    }

    /* TODO missing examples... */

    def `serve ++=` {
      val (gBefore, gAfter) = (Graph(1, 2 ~ 3), Graph(0, 1 ~ 2, 2 ~ 3))
      (gBefore ++= (0 :: Nil, List(1 ~ 2, 2 ~ 3))) should equal(gAfter)
      (gBefore ++= Graph(0, 1 ~ 2)) should equal(gAfter)
      (gBefore ++= Graph[Int, UnDiEdge](0) ++= Graph(1 ~ 2)) should equal(gAfter)
    }

    /* TODO missing examples... */
  }
}

class TEdit[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: ConfigWrapper[CC])
    extends RefSpec
    with Matchers {

  info("factory = " + factory.companion.getClass)

  implicit private val config: factory.companion.Config = factory.config

  private val seq_1_3   = Seq(1, 3)
  private val gInt_1_3  = factory(seq_1_3.toOuterElems[DiEdge[Int]]: _*)
  private val gString_A = factory("A")

  object `graph editing` {
    def `empty ` {
      val eg = factory.empty[Nothing, Nothing]
      eg shouldBe empty
      eg should have size 0
    }

    def `apply ` {
      gInt_1_3 should not be empty
      gInt_1_3.order should be(2)
      gInt_1_3(0) should be(false)
      gInt_1_3(1) should be(true)
      gInt_1_3(2) should be(false)
      gInt_1_3(3) should be(true)

      val r = HyperEdge(1, 2, 3)
      val g = factory[Int, AnyHyperEdge](1, r, 1 ~ 2)
      g.nodes should have size 3
      g.edges should have size 2
      g.elementCount should be(5)
      g.contains(r) should be(true)

      val h = factory(UnDiEdge(1, 2), UnDiEdge(2, 3))
      h.nodes should have size 3
      h.edges should have size 2
      h.elementCount should be(5)
    }

    def `nodes of ADT fixes #40` {
      trait Node
      case class N1() extends Node
      case class N2() extends Node
      factory(N1() ~> N2(), N1() ~> N1())
    }

    /* TODO
    def `isDirected ` {
      def directed(g: CC[Int, UnDiEdge], expected: Boolean): Unit = g.isDirected should be(expected)
      val wDi                                                     = edge.WDiEdge(1, 2)(0)

      factory(wDi).isDirected should be(true)
      directed(factory(1 ~ 2), false)
      directed(factory(1 ~> 2), true)
      directed(factory(wDi), true)
      directed(factory(0 ~> 1, wDi), true)
    }
     */
    def `isHyper ` {
      def test(g: CC[Int, AnyHyperEdge[Int]], expected: Boolean): Unit = g.isHyper should be(expected)

      import HyperEdgeImplicits._
      test(factory.from[Int, AnyHyperEdge](List(1 ~> 2, 1 ~~ 2 ~~ 3)), true)
      test(factory.from[Int, AnyHyperEdge](1 ~ 2 :: Nil), false)
      test(factory.from[Int, AnyHyperEdge](1 ~> 2 :: Nil), false)
    }

    /* TODO missing examples... */

    def `from ` {
      val (n_start, n_end) = (11, 20)
      val nodes            = List.range(n_start, n_end)
      val edges            = List[DiEdge[Int]](14 ~> 16, 16 ~> 18, 18 ~> 20, 20 ~> 22)
      val g                = factory.from(nodes, edges)
      g.nodes.size should be(nodes.size + 2)
      g.edges.size should be(edges.size)
    }

    def `contains ` {
      seq_1_3 foreach (n => gInt_1_3 contains n should be(true))
      gInt_1_3.iterator.next shouldBe a[gInt_1_3.InnerNode]
    }

    def `toString ` {
      gInt_1_3.toString should fullyMatch regex """Graph\([1], [3]\)"""
      gString_A.toString should fullyMatch regex """Graph\(["A"]\)"""
    }

    def `from inner ` {
      val gn = factory(2, 3)
      factory.from[Int, Nothing](gn.nodes.toOuter, Nil) should equal(gn)

      val g = factory(2 ~ 3)
      factory(g.edges.head) should equal(g)
      factory.from(g.edges.toOuter) should equal(g)
    }

    def `NodeSet ` {
      val o = Vector.range(0, 4)
      val g = factory(o(1) ~ o(2), o(2) ~ o(3))
      val n = o map (g.nodes find _ getOrElse g.nodes.head)

      val less = g.nodes - n(3)
      less should have size 2
      less should contain(n(1))
      less.find(_ == n(1)).get.edges should have size 1
      less should contain(n(2))
      less.find(_ == n(2)).get.edges should have size 2

      val restored = less + n(3)
      restored should have size 3
      restored should contain(n(3))
      restored.find(_ == n(1)).get.edges should have size 1
    }

    def `Eq ` {
      factory[Int, Nothing]() shouldEqual factory[Int, DiEdge]()
      gInt_1_3 shouldEqual factory(1, 3)
      gString_A shouldEqual factory("A")

      factory() shouldNot be(factory(1))
      gInt_1_3 shouldNot be(factory(2, 3))
      gString_A shouldNot be(factory("B"))

      gInt_1_3 should be(immutable.Graph(1) + 3)
    }

    def `EdgeAssoc ` {
      val e = 1 ~ 2
      e shouldBe an[UnDiEdge[_]]
      val x = factory(3 ~ 4).nodes
      // Error in Scala compiler: assertion failed
      // Graph(3).nodes contains 3 //should be (true)

      val d = 1 ~> 2
      d shouldBe a[DiEdge[_]]
      d.source should be(1)
      d.target should be(2)
      factory[Int, AnyEdge](1, d, 1 ~ 4).nodes should have size (3)

      {
        val heNodes = List("A", "B", "C")
        val he      = HyperEdge(heNodes)

        he shouldBe a[HyperEdge[_]]
        he.arity should be(heNodes.size)
        he._1 should be(heNodes(0))
        he._2 should be(heNodes(1))
        for (i <- heNodes.indices) he._n(i) should be(heNodes(i))
      }

      {
        import DiHyperEdgeImplicits._
        val sources = List('A', 'B', 'C')
        val target  = 'D'
        val dhe     = sources ~~> target

        dhe shouldBe a[DiHyperEdge[_]]
        dhe.ends should contain theSameElementsAs (target +: sources)
        dhe.arity should be(4)
        dhe.sources.head should be(sources.head)
        dhe.sources.last should be(sources.last)
        dhe.targets.head should be(target)
      }
    }

    private val gUnDi  = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
    private val gDi    = factory(1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
    private val gMixed = factory[Int, AnyEdge](1 ~> 2, 2 ~> 3, 4 ~ 3)
    private val hDi = {
      import DiHyperEdgeImplicits._
      factory(1 ~~> List(1, 5), 1 ~~> List(2, 5), 1 ~~> List(3, 5), 1 ~~> List(4, 9))
    }

    object `diSuccessors ` {
      def `for UnDi` {
        (gUnDi get 1).diSuccessors should be(Set(2, 3, 4))
        (gUnDi get 2).diSuccessors should be(Set(1))
      }
      def `for Di` {
        (gDi get 1).diSuccessors should be(Set(2, 3, 4))
        (gDi get 2).diSuccessors should be(Set.empty)
      }
      def `for mixed` {
        (gMixed get 2).diSuccessors should be(Set(3))
        (gMixed get 3).diSuccessors should be(Set(4))
      }
      def `for DiHyper` {
        (hDi get 1).diSuccessors should be(Set(2, 3, 4, 5, 9))
        (hDi get 2).diSuccessors should be(Set.empty)
        (hDi get 5).diSuccessors should be(Set.empty)
      }
    }

    object `diPredecessors ` {
      def `for UnDi` {
        (gUnDi get 1).diPredecessors should be(Set(2, 3, 4))
        (gUnDi get 2).diSuccessors should be(Set(1))
      }
      def `for Di` {
        (gDi get 1).diPredecessors should be(Set.empty)
        (gDi get 2).diPredecessors should be(Set(1))
      }
      def `for mixed` {
        (gMixed get 2).diPredecessors should be(Set(1))
        (gMixed get 3).diSuccessors should be(Set(4))
      }
      def `for DiHyper` {
        (hDi get 1).diPredecessors should be(Set.empty)
        (hDi get 2).diPredecessors should be(Set(1))
        (hDi get 5).diPredecessors should be(Set(1))
      }
    }

    object `neighbors ` {
      def `for UnDi` {
        (gUnDi get 1).neighbors should be(Set(2, 3, 4))
        (gUnDi get 2).neighbors should be(Set(1))
      }
      def `for Di` {
        (gDi get 1).neighbors should be(Set(2, 3, 4))
        (gDi get 2).neighbors should be(Set(1))
      }
      def `for DiHyper` {
        (hDi get 1).neighbors should be(Set(2, 3, 4, 5, 9))
        (hDi get 2).neighbors should be(Set(1, 5))
        (hDi get 5).neighbors should be(Set(1, 2, 3))
      }
    }

    def `findOutgoingTo Di` {
      val g         = factory(1 ~> 1, 1 ~> 2, 2 ~> 1)
      def n(i: Int) = g get i
      (n(1) findOutgoingTo n(2)) should be(Some(1 ~> 2))
      (n(1) findOutgoingTo n(1)) should be(Some(1 ~> 1))
    }

    def `degree ` {
      val g = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
      (g get 1).degree should be(5)
      (g get 2).degree should be(1)
    }

    def `incoming ` {
      val uEdges = Seq[UnDiEdge[Int]](1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4) // bug if no type param given
      val g      = factory(uEdges(0), uEdges(1), uEdges(2), uEdges(3))
      (g get 1).incoming should be(uEdges.toSet)
      (g get 2).incoming should be(Set(uEdges(1)))

      val dEdges = Seq[DiEdge[Int]](1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
      val h      = factory(dEdges(0), dEdges(1), dEdges(2), dEdges(3))
      (h get 1).incoming should be(Set(dEdges(0)))
      (h get 2).incoming should be(Set(dEdges(1)))
    }

    def `edgeAdjacents UnDi` {
      val g = factory[Int, AnyEdge](1 ~ 2, 2 ~ 3, 1 ~> 3, 1 ~ 5, 3 ~ 5, 3 ~ 4, 4 ~> 4, 4 ~> 5)
      (g get 4 ~> 4).adjacents should be(Set[AnyEdge[Int]](3 ~ 4, 4 ~> 5))
      (g get 1 ~ 2).adjacents should be(Set[AnyEdge[Int]](1 ~> 3, 1 ~ 5, 2 ~ 3))
    }

    def `filter ` {
      val g = factory(2 ~> 3, 3 ~> 1, 5)
      g filter (_ > 1) should be(factory(2 ~> 3, 5))
      g filter (_ < 2) should be(factory(1))
      g filter (_ < 2) should be(factory(1))
      g filter (_ >= 2) should be(factory(2 ~> 3, 5))
      g filter (fEdge = _._1 == 2) should be(factory(1, 5, 2 ~> 3))
      g filter (fNode = _ <= 3, fEdge = _ contains 2) should be(factory(1, 2 ~> 3))
    }

    def `match ` {
      val di = 1 ~> 2
      (di match { case DiEdge(src, _) => src }) should be(1)
      (di match { case src ~> trg     => src + trg }) should be(3)

      val unDi = 1 ~ 2
      (unDi match { case UnDiEdge(n1, _) => n1 }) should be(1)
      (unDi match { case n1 ~ n2         => n1 + n2 }) should be(3)

      {
        import HyperEdgeImplicits._
        val hyper = 1 ~~ 2 ~~ 3
        (hyper match { case HyperEdge(Seq(n1, n2, n3, _ @_*)) => n1 + n2 + n3 }) should be(6)
// TODO (hyper match { case HyperEdge(n1 ~~ (n2, n3)            => n1 + n2 + n3 }) should be(6)
      }

      {
        import DiHyperEdgeImplicits._
        val count   = 3
        val sources = List.tabulate(count - 1)(_ + 1)
        val target  = count
        val diHyper = sources ~~> target
        (diHyper match { case DiHyperEdge(Seq(s1, _*), _) => s1 }) should be(sources.head)
        (diHyper match { case _ ~~> targets               => targets.head }) should be(target)
      }
    }
  }
}
