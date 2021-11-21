package scalax.collection

import language.postfixOps
import scala.reflect.ClassTag

import GraphPredef._, GraphEdge._
import generic.GraphCompanion

import org.scalatest._
import org.scalatest.matchers.should
import org.scalatest.refspec.RefSpec

/** This wrapper trait enables to transparently pass `GraphCompanion` objects with
  *  non-default configuration parameters to tests in a type-safe way.
  */
trait ConfigWrapper[CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] {
  val companion: GraphCompanion[CC]
  implicit val config: companion.Config

  def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: companion.Config): CC[N, E] =
    companion.empty

  def apply[N, E[+X] <: EdgeLikeIn[X]](
      elems: Param[N, E]*
  )(implicit edgeT: ClassTag[E[N]], config: companion.Config): CC[N, E] =
    companion(elems: _*)

  def from[N, E[+X] <: EdgeLikeIn[X]](
      edges: collection.Iterable[E[N]]
  )(implicit edgeT: ClassTag[E[N]], config: companion.Config): CC[N, E] =
    companion.from(edges = edges)

  def from[N, E[+X] <: EdgeLikeIn[X]](nodes: collection.Iterable[N], edges: collection.Iterable[E[N]])(implicit
      edgeT: ClassTag[E[N]],
      config: companion.Config
  ): CC[N, E] = companion.from(nodes, edges)
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

class TEditImmutable extends RefSpec with should.Matchers {
  object `graphs ` {
    def `are immutable by default`: Unit = {
      val g = Graph[Nothing, Nothing]()
      g.isInstanceOf[immutable.Graph[Nothing, Nothing]] should be(true)
    }

    def `yield another graph when mapped`: Unit = {
      val g                                 = immutable.Graph(1 ~ 2)
      val m: immutable.Graph[Int, UnDiEdge] = g map Helper.incrementNode
      m.edges.head should be(UnDiEdge(2, 3))
    }
  }
}

class TEditMutable extends RefSpec with should.Matchers {
  object `mutable graphs` {
    def `serve += properly`: Unit = {
      val g = mutable.Graph[Int, Nothing](1, 3)
      g += 2
      g should have size 3
      for (i <- 1 to 3)
        g.contains(i) should be(true) //g should contain (i)
    }

    def `serve -= properly`: Unit = {
      val g = mutable.Graph(1, 2, 2 ~ 3, 4)
      g remove 1 should be(true)
      g should be(mutable.Graph(2 ~ 3, 4))
      g remove 5 should be(false)
      g minusIsolated 2 should be(g)
      (g -?= 2) should be(g)
      (g -= 2) should be(mutable.Graph[Int, UnDiEdge](3, 4))
      g.clear
      g should be('empty)
    }

    def `serve -= properly (2)` : Unit = {
      val g = mutable.Graph(1 ~ 2, 2 ~ 3)
      (g -= 2) should be(mutable.Graph[Int, UnDiEdge](1, 3))
      g.graphSize should be(0)
    }

    def `serve 'directed' properly`: Unit = {
      val (di, unDi) = (1 ~> 2, 2 ~ 3)
      val g          = mutable.Graph(unDi)

      def directed(expected: Boolean): Unit = g.isDirected should be(expected)

      directed(false)
      g.clear;
      directed(true)
      g += di;
      directed(true)
      g += unDi;
      directed(false)
      g.clear;
      directed(true)
    }

    def `serve size`: Unit = {
      val g0 = mutable.Graph.empty
      (g0.size, g0.graphSize) should be(0, 0)

      val g1 = mutable.Graph(1)
      (g1.size, g1.graphSize) should be(1, 0)

      val g2 = mutable.Graph(1, 2)
      (g2.size, g2.graphSize) should be(2, 0)

      val g3 = mutable.Graph(1 ~> 2)
      (g3.size, g3.graphSize) should be(3, 1)

      val g4 = mutable.Graph(1 ~> 2, 3)
      (g4.size, g4.graphSize) should be(4, 1)

      val g5 = mutable.Graph(1 ~> 2, 2 ~ 3)
      (g5.size, g5.graphSize) should be(5, 2)
    }

    def `serve clear`: Unit = {
      type G = mutable.Graph[Int, UnDiEdge]

      def checkSizeAroundClear(g: G, beforeSize: Int) = {
        g.size should be(beforeSize)
        g.clear()
        g.nodes.size should be(0)
        g.edges.size should be(0)
        g.size should be(0)
        g should be(empty)
      }

      val g0: G = mutable.Graph.empty
      checkSizeAroundClear(g0, 0)

      val g1: G = mutable.Graph(1)
      checkSizeAroundClear(g1, 1)

      val g2: G = mutable.Graph(1, 2)
      checkSizeAroundClear(g2, 2)

      val g3: G = mutable.Graph(1 ~> 2)
      checkSizeAroundClear(g3, 3)

      val g4: G = mutable.Graph(1 ~> 2, 3)
      checkSizeAroundClear(g4, 4)

      val g5: G = mutable.Graph(1 ~> 2, 2 ~ 3)
      checkSizeAroundClear(g5, 5)
    }

    def `serve 'diSuccessors' when directed`: Unit = {
      val (one, two, oneOne, oneTwo) = (1, 2, 1 ~> 1, 1 ~> 2)
      val g                          = mutable.Graph(oneOne, oneTwo, one ~> 3, one ~> 4)
      val (n1, n2)                   = (g get one, g get two)
      val e11                        = g get oneOne

      g -= 1 ~> 4 // Graph(oneOne, oneTwo, one~>3)
      (n2 diSuccessors) should be('isEmpty)
      (n1 diSuccessors) should be(Set(two, 3))
      (n1 ~>? n1) should be(Some(e11))

      g -= oneTwo // Graph(oneOne, one~>3)
      (n1 diSuccessors) should be(Set(3))
      (n1 ~>? n1) should be(Some(e11))

      g -= oneOne // Graph(one~>3)
      (n1 diSuccessors) should be(Set(3))
      (n1 ~>? n1) should be(None)

      g ++= List(oneOne, oneTwo) // Graph(oneOne, oneTwo, one~>3)
      (n1 diSuccessors) should be(Set(two, 3))
      (n1 ~>? n1) should be(Some(e11))
    }

    def `'diSuccessors' when directed hypergraph`: Unit = {
      val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, 1 ~> 1 ~> 2, 1 ~> 2 ~> 3)
      val g                                         = mutable.Graph(oneOneTwo, oneTwoThree)
      val (n1, n2)                                  = (g get one, g get two)

      (n2 diSuccessors) should be('isEmpty)
      (n1 diSuccessors) should be(Set(two, three))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g -= oneTwoThree // Graph(oneOneTwo)
      (n1 diSuccessors) should be(Set(two))
      (n1 ~>? n1) should be(Some(oneOneTwo))

      g -= two // Graph(one)
      (n1 diSuccessors) should be('isEmpty)
      (n1 ~>? n1) should be(None)

      g += oneOneTwo // Graph(oneOneTwo)
      (n1 diSuccessors) should be(Set(2))
      (n1 ~>? n1) should be(Some(oneOneTwo))
    }

    def `serve +~` : Unit = {
      val g = mutable.Graph(2 ~ 3)

      def n(i: Int) = g get i

      implicit val unDiFactory = UnDiEdge
      g addEdge (n(3), n(2)) should be(false)
      (g +~= (n(2), n(2))) should have size 4

      g.addAndGetEdge(2, 3)(DiEdge).directed should be(true)
      g should have('order (2), 'graphSize (3))

      n(3) +~ 4
      g should have('order (3), 'graphSize (4))

      (n(3) +~ n(2))(DiEdge)
      g should have('order (3), 'graphSize (5))
    }

    def `serve +~ for hyperedeges`: Unit = {
      implicit val factory = HyperEdge
      val h                = mutable.Graph(1 ~ 1 ~ 2)
      h should have('order (2), 'graphSize (1))
      h +~= (0, 1, 2, 3)
      h should have('order (4), 'graphSize (2))
      h +~= (11, 12, 13, 14, 15, 16) // edge with at least 6 endpoints will trigger use of NodeProduct
      h should have('order (10), 'graphSize (3))
    }

    def `serve +~%= for weighted edeges`: Unit = {
      val g          = mutable.Graph(2 ~ 3)
      implicit val f = edge.WUnDiEdge
      g.addWEdge(3, 4)(2)
      g should have('order (3), 'graphSize (2), 'totalWeight (3))
      (g +~%= (2, 4))(3)
      g should have('order (3), 'graphSize (3), 'totalWeight (6))
      // (g +~%= (0,1,2,3))(3)(edge.WHyperEdge) // must not compile
    }

    def `serve +~%= weighted hyperedeges`: Unit = {
      implicit val factory = edge.WHyperEdge
      val h                = mutable.Graph(1 ~ 1 ~ 2)
      h should have('order (2), 'graphSize (1))
      h.addWEdge(3, 4, 5)(2)
      h should have('order (5), 'graphSize (2), 'totalWeight (3))
      (h +~%= (0, 1, 2, 3))(3)
      h should have('order (6), 'graphSize (3), 'totalWeight (6))
    }

    def `fulfill labeled edege equality`: Unit = {
      import edge.Implicits._
      import edge.LDiEdge

      type StringLabel = Option[String]
      val str                = "A"
      val label: StringLabel = Some(str)
      val g                  = mutable.Graph(2 ~ 3, (2 ~+# 3)(label))
      g should have('order (2), 'graphSize (2))

      import edge.LBase.{LEdgeImplicits}
      object StringLabelImplicit extends LEdgeImplicits[StringLabel]
      import StringLabelImplicit._
      for (e <- g.edges if e.isLabeled) {
        e.isDefined should be(true)
        e.get should be(str)
      }

      type ListLabel = List[Int]
      implicit val factory = LDiEdge
      val listLabel        = List(1, 0, 1)
      g.addLEdge(3, 4)(listLabel) should be(true)
      g should have('order (3), 'graphSize (3))
      val findAdded = g.edges find (3 ~> 4)
      findAdded should be('isDefined)
      val added: g.EdgeT = findAdded.get
      added.directed should be(true)
      added.count(_ > 0) should be(List(1, 0, 1).count(_ > 0))
    }

    def `fulfill labeled directed hyperedege equality`: Unit = {
      import edge.Implicits._
      import edge.LHyperEdge

      type StringLabel = String
      val outerLabels = Seq("A", "BC", "CDE")
      val g           = mutable.Graph(1 ~ 2 ~ 3, (2 ~+# 3)(outerLabels(0)))

      implicit val factory = LHyperEdge
      (g +~+= (3, 4, 5))(outerLabels(1))
      g should have('order (5), 'graphSize (3))
      g.addLEdge(4, 5, 6)(outerLabels(2)) should be(true)
      g should have('order (6), 'graphSize (4))

      val innerLabels: collection.mutable.Set[_ >: StringLabel] =
        g.edges filter (_.isLabeled) map (_.label)
      innerLabels should have size (outerLabels.size)
      /*
      innerLabels forall (outerLabels contains _) should be (true)
       * https://groups.google.com/forum/?fromgroups=#!searchin/scala-internals/both$20method/scala-internals/nPZY2EMtDvY/PivCCtyRM_IJ
       * https://issues.scala-lang.org/browse/SI-5330
       */
      (innerLabels: Iterable[Any]) forall (outerLabels contains _) should be(true)
    }

    def `serve ++=` : Unit = {
      val (gBefore, gAfter) = (mutable.Graph(1, 2 ~ 3), mutable.Graph(0, 1 ~ 2, 2 ~ 3))
      (gBefore ++= List[Param[Int, UnDiEdge]](1 ~ 2, 2 ~ 3, 0)) should equal(gAfter)
      (gBefore ++= mutable.Graph(0, 1 ~ 2)) should equal(gAfter)
      (gBefore ++= mutable.Graph[Int, UnDiEdge](0)
        ++= mutable.Graph(1 ~ 2)) should equal(gAfter)
    }

    def `are upsertable`: Unit = {
      import edge.LDiEdge
      val (label, modLabel) = ("A", "B")
      val g                 = mutable.Graph(LDiEdge(1, 2)(label), LDiEdge(2, 3)(label))

      g.edges foreach {
        _.edge match {
          case LDiEdge(s, t, l) => g upsert (LDiEdge(s.value, t.value)(modLabel))
        }
      }
      g should have('graphSize (2))
      g.edges foreach {
        _.label should be(modLabel)
      }
    }

    def `yield another graph when mapped`: Unit = {
      import mutable.Graph
      val g                       = Graph(1 ~ 2)
      val m: Graph[Int, UnDiEdge] = g map Helper.incrementNode
      m.edges.head should be(UnDiEdge(2, 3))
    }
  }
}

class TEdit[CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: ConfigWrapper[CC])
    extends RefSpec
    with should.Matchers {

  info("factory = " + factory.companion.getClass)

  implicit val config: factory.companion.Config = factory.config

  val seq_1_3: Seq[Int]              = Seq(1, 3)
  val gInt_1_3: CC[Int, DiEdge]      = factory(seq_1_3.toOuterNodes[DiEdge]: _*)
  val gString_A: CC[String, Nothing] = factory[String, Nothing]("A")

  object `graph editing includes` {
    def `empty ` : Unit = {
      val eg = factory.empty[Nothing, Nothing]
      eg should be('isEmpty)
      eg should have size 0
      eg should equal(eg.empty)
    }

    def `apply ` : Unit = {
      gInt_1_3 should not be 'isEmpty
      gInt_1_3 should have size 2
      gInt_1_3(0) should be(false)
      gInt_1_3(1) should be(true)
      gInt_1_3(2) should be(false)
      gInt_1_3(3) should be(true)

      val r = HyperEdge(1, 2, 3)
      val g = factory(1, r, 1 ~ 2)
      g.nodes should have size 3
      g.edges should have size 2
      g should have size 5
      g.contains(r) should be(true)

      val h = factory(UnDiEdge(1, 2), UnDiEdge(2, 3))
      h.nodes should have size 3
      h.edges should have size 2
      h should have size 5
    }

    def `nodes of ADT fixes #40`: Unit = {
      trait Node
      case class N1() extends Node
      case class N2() extends Node
      factory(N1() ~> N2(), N1() ~> N1())
    }

    def `isDirected ` : Unit = {
      def directed(g: CC[Int, UnDiEdge], expected: Boolean): Unit = g.isDirected should be(expected)

      val wDi = edge.WDiEdge(1, 2)(0)

      factory(wDi).isDirected should be(true)
      directed(factory(1 ~ 2), false)
      directed(factory(1 ~> 2), true)
      directed(factory(wDi), true)
      directed(factory(0 ~> 1, wDi), true)
    }

    def `isHyper ` : Unit = {
      def hyper(g: CC[Int, HyperEdge], expected: Boolean): Unit = g.isHyper should be(expected)

      factory(1 ~ 2).isHyper should be(false)
      hyper(factory(1 ~> 2, 1 ~ 2 ~ 3), true)
      hyper(factory(1 ~> 2 ~> 3 ~> 4 ~> 5 ~> 6), true)
      hyper(factory(1 ~> 2), false)
    }

    def `isMulti ` : Unit = {
      import edge.WkDiEdge
      def multi(g: CC[Int, UnDiEdge], expected: Boolean): Unit = g.isMulti should be(expected)

      val (wDi_1, wDi_2) = (WkDiEdge(1, 2)(0), WkDiEdge(1, 2)(1))

      multi(factory(1 ~ 2), false)
      multi(factory(1 ~ 2, 1 ~> 2), false)
      multi(factory(wDi_1, wDi_2), true)
    }

    def `from ` : Unit = {
      val (n_start, n_end) = (11, 20)
      val nodes            = List.range(n_start, n_end)
      val edges            = List[DiEdge[Int]](14 ~> 16, 16 ~> 18, 18 ~> 20, 20 ~> 22)
      val g                = factory.from(nodes, edges)
      g.nodes.size should be(nodes.size + 2)
      g.edges.size should be(edges.size)
    }

    def `contains ` : Unit = {
      seq_1_3 foreach { i =>
        gInt_1_3.contains(i) should be(true) //gInt_1_3 should contain (i)
      }
      gInt_1_3.head.isInstanceOf[InnerNodeParam[Int]] should be(true)
    }

    def `toString ` : Unit = {
      val nodePrefix = OuterNode.stringPrefix
      gInt_1_3.toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """[13], """ + nodePrefix + """[13]\)""")
      gString_A.toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """["A"]\)""")
    }

    def `from inner ` : Unit = {
      val gn = factory[Int, UnDiEdge](2, 3)
      factory(gn.nodes: _*) should equal(gn)
      factory.from[Int, Nothing](gn.nodes, Nil) should equal(gn)

      val g = factory(2 ~ 3)
      factory(g.edges.head) should equal(g)
      factory(g.edges: _*) should equal(g)
      factory.from[Int, UnDiEdge](edges = g.edges) should equal(g)
    }

    def `+ Int`: Unit = {
      val g = factory(1, 2 ~ 3)
      g + 1 should be(g)
      g + 0 should be(Graph(0, 1, 2, 3, 2 ~ 3))
      g + 0 ~ 1 should be(Graph(0, 1, 2, 3, 0 ~ 1, 2 ~ 3))
      //g + "A" !!! // error: type mismatch
    }

    def `+ String ` : Unit = {
      val g = gString_A + "B"
      g should have size 2
      g.contains("A") should be(true) //g should contain ("A")
      g.contains("B") should be(true) //g should contain ("B")

      val hString_A = factory[String, UnDiEdge]("A")
      val h         = hString_A + ("A" ~ "C")
      h.nodes should have size 2
      h.edges should have size 1
      h should have size 3
    }

    def `++ ` : Unit = {
      val g = gString_A + "B" + "C"
      g should have size 3
      g.contains("A") should be(true) //g should contain ("A")
      g.contains("B") should be(true) //g should contain ("B")
      g.contains("C") should be(true) //g should contain ("C")

      val (gBefore, gAfter) = (factory(1, 2 ~ 3), factory(0, 1 ~ 2, 2 ~ 3))
      gBefore ++ List[Param[Int, UnDiEdge]](1 ~ 2, 2 ~ 3, 0) should equal(gAfter)
      gBefore ++ factory(0, 1 ~ 2) should equal(gAfter)
      gBefore ++ factory[Int, UnDiEdge](0) ++ factory(1 ~ 2) should equal(gAfter)
    }

    def `- ` : Unit = {
      var g = gString_A - "B"
      g should have size 1

      g = gString_A - "A"
      g.contains("A") should be(false) //gMinus should not contain ("A")
      g should have size 0
      g should be('isEmpty)

      val h = factory(1, 2, 2 ~ 3)
      h - 0 should be(h)
      h - 1 should be(factory(2, 2 ~ 3))
      h minusIsolated 2 should be(h)
      h - 2 should be(factory[Int, UnDiEdge](1, 3))
    }

    def `-- ` : Unit = {
      val g = factory(1, 2 ~ 3, 3 ~ 4)
      g -- List[Param[Int, UnDiEdge]](2, 3 ~ 3) should be(factory(1, 3 ~ 4))
      g -- List[Param[Int, UnDiEdge]](2, 3 ~ 4) should be(factory[Int, UnDiEdge](1, 3, 4))
      g --! List[Param[Int, UnDiEdge]](1, 3 ~ 4) should be(factory(2 ~ 3))
    }

    def `CanBuildFrom UnDi`: Unit = {
      val g                       = factory(0, 1 ~ 2)
      val m: Graph[Int, UnDiEdge] = g map Helper.incrementNode
      m find 1 should be('defined)
      m.edges.head should be(UnDiEdge(2, 3))
    }

    def `CanBuildFrom Di`: Unit = {
      val g                          = factory(1 ~> 2)
      val m: Graph[String, UnDiEdge] = g map Helper.nodeToString
      m.edges.head should be("1" ~ "2")
    }

    def `NodeSet ` : Unit = {
      val o = Range(0, 4)
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

    def `Eq ` : Unit = {
      factory[Int, Nothing]() shouldEqual factory[Int, Nothing]()
      gInt_1_3 shouldEqual factory(seq_1_3.toOuterNodes[DiEdge]: _*)
      gString_A shouldEqual factory[String, Nothing]("A")

      factory[Int, Nothing]() should not be (factory[Int, Nothing](1))
      gInt_1_3 should not be (factory[Int, DiEdge](2, 3))
      gString_A should not be (factory[String, Nothing]("B"))

      gInt_1_3 should be(factory[Int, DiEdge](1) + 3)
    }

    def `EdgeAssoc ` : Unit = {
      val e = 1 ~ 2
      e.isInstanceOf[UnDiEdge[Int]] should be(true)
      // Error in Scala compiler: assertion failed
      // Graph(3).nodes contains 3 //should be (true)

      val d = 1 ~> 2
      d.isInstanceOf[DiEdge[Int]] should be(true)
      d.source should be(1)
      d.target should be(2)
      factory(1, d, 1 ~ 4).nodes should have size 3

      val heNodes = List("A", "B", "C")
      val he      = heNodes(0) ~ heNodes(1) ~ heNodes(2)
      he.isInstanceOf[HyperEdge[String]] should be(true)
      he.arity should be(heNodes.size)
      he._1 should be(heNodes(0))
      he._2 should be(heNodes(1))
      for (i <- 0 to (heNodes.size - 1))
        he._n(i) should be(heNodes(i))

      val dhe = "A" ~> "B" ~> "C" ~> "D"
      dhe.isInstanceOf[DiHyperEdge[String]] should be(true)
      dhe.arity should be(4)
      dhe._1 should be("A")
      dhe._n(3) should be("D")
    }
    object `diSuccessors ` {
      def `for UnDi`: Unit = {
        val g = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
        (g get 1 diSuccessors) should be(Set(2, 3, 4))
        (g get 2 diSuccessors) should be(Set(1))
      }

      def `for Di`: Unit = {
        val g = factory(1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
        (g get 1 diSuccessors) should be(Set(2, 3, 4))
        (g get 2 diSuccessors) should be(Set.empty)
      }

      def `for mixed`: Unit = {
        val g = factory(1 ~> 1, 2 ~> 3, 4 ~ 3)
        (g get 2 diSuccessors) should be(Set(3))
        (g get 3 diSuccessors) should be(Set(4))
      }

      def `for DiHyper`: Unit = {
        val h = factory(1 ~> 1 ~> 5, 1 ~> 2 ~> 5, 1 ~> 3 ~> 5, 1 ~> 4 ~> 9, 11 ~> 12 ~> 13 ~> 14 ~> 15 ~> 16)
        (h get 1 diSuccessors) should be(Set(2, 3, 4, 5, 9))
        (h get 2 diSuccessors) should be(Set.empty)
        (h get 5 diSuccessors) should be(Set.empty)
        (h get 11 diSuccessors) should be(Set(12, 13, 14, 15, 16))
        (h get 12 diSuccessors) should be(Set.empty)
      }
    }
    object `diPredecessors ` {
      def `for UnDi`: Unit = {
        val g = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
        (g get 1 diPredecessors) should be(Set(2, 3, 4))
        (g get 2 diPredecessors) should be(Set(1))
      }

      def `for Di`: Unit = {
        val g = factory(1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
        (g get 1 diPredecessors) should be(Set.empty)
        (g get 2 diPredecessors) should be(Set(1))
      }

      def `for mixed`: Unit = {
        val g = factory(1 ~> 2, 2 ~> 3, 4 ~ 3)
        (g get 2 diPredecessors) should be(Set(1))
        (g get 3 diSuccessors) should be(Set(4))
      }

      def `for DiHyper`: Unit = {
        val h = factory(1 ~> 1 ~> 5, 1 ~> 2 ~> 5, 1 ~> 3 ~> 5, 1 ~> 4 ~> 9, 11 ~> 12 ~> 13 ~> 14 ~> 15 ~> 16)
        (h get 1 diPredecessors) should be(Set.empty)
        (h get 2 diPredecessors) should be(Set(1))
        (h get 5 diPredecessors) should be(Set(1))
        (h get 12 diPredecessors) should be(Set(11))
        (h get 16 diPredecessors) should be(Set(11))
      }
    }
    object `neighbors ` {
      def `for UnDi`: Unit = {
        val g = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
        (g get 1 neighbors) should be(Set(2, 3, 4))
        (g get 2 neighbors) should be(Set(1))
      }

      def `for Di`: Unit = {
        val g = factory(1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
        (g get 1 neighbors) should be(Set(2, 3, 4))
        (g get 2 neighbors) should be(Set(1))
      }

      def `for DiHyper`: Unit = {
        val h = factory(1 ~> 1 ~> 5, 1 ~> 2 ~> 5, 1 ~> 3 ~> 5, 1 ~> 4 ~> 9, 11 ~> 12 ~> 13 ~> 14 ~> 15 ~> 16)
        (h get 1 neighbors) should be(Set(2, 3, 4, 5, 9))
        (h get 2 neighbors) should be(Set(1, 5))
        (h get 5 neighbors) should be(Set(1, 2, 3))
        (h get 11 neighbors) should be(Set(12, 13, 14, 15, 16))
        (h get 15 neighbors) should be(Set(11, 12, 13, 14, 16))
      }
    }

    def `findOutgoingTo Di`: Unit = {
      val g = factory(1 ~> 1, 1 ~> 2, 2 ~> 1)

      def n(i: Int) = g get i

      (n(1) findOutgoingTo n(2)) should be(Some(1 ~> 2))
      (n(1) findOutgoingTo n(1)) should be(Some(1 ~> 1))
    }

    def `degree ` : Unit = {
      val g = factory(1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4)
      (g get 1 degree) should be(5)
      (g get 2 degree) should be(1)
    }

    def `incoming ` : Unit = {
      val uEdges = Seq[UnDiEdge[Int]](1 ~ 1, 1 ~ 2, 1 ~ 3, 1 ~ 4) // bug if no type param given
      val g      = factory(uEdges(0), uEdges(1), uEdges(2), uEdges(3))
      (g get 1 incoming) should be(uEdges.toSet)
      (g get 2 incoming) should be(Set(uEdges(1)))

      val dEdges = Seq[DiEdge[Int]](1 ~> 1, 1 ~> 2, 1 ~> 3, 1 ~> 4)
      val h      = factory(dEdges(0), dEdges(1), dEdges(2), dEdges(3))
      (h get 1 incoming) should be(Set(dEdges(0)))
      (h get 2 incoming) should be(Set(dEdges(1)))
    }

    def `edgeAdjacents UnDi`: Unit = {
      val g = Graph(1 ~ 2, 2 ~ 3, 1 ~> 3, 1 ~ 5, 3 ~ 5, 3 ~ 4, 4 ~> 4, 4 ~> 5)
      ((g get 4 ~> 4) adjacents) should be(Set(3 ~ 4, 4 ~> 5))
      ((g get 1 ~ 2) adjacents) should be(Set(1 ~> 3, 1 ~ 5, 2 ~ 3))
    }

    def `filter ` : Unit = {
      val g = factory(2 ~> 3, 3 ~> 1, 5)
      g filter ((n: Int) => n > 1) should be(factory(2 ~> 3, 5))
      g filter ((n: Int) => n < 2) should be(factory[Int, DiEdge](1))
      g filter g.having(node = _ < 2) should be(factory[Int, DiEdge](1))
      g filter g.having(node = _ >= 2) should be(factory(2 ~> 3, 5))
      g filter g.having(edge = _._1 == 2) should be(factory(2 ~> 3))
      g filter g.having(edge = _ contains 2) should be(factory(2 ~> 3))
    }

    def `match ` : Unit = {
      val di = 1 ~> 2
      (di match {
        case DiEdge(src, _) => src
      }) should be(1)
      (di match {
        case src ~> trg => src + trg
      }) should be(3)

      val unDi = 1 ~ 2
      (unDi match {
        case UnDiEdge(n1, _) => n1
      }) should be(1)
      (unDi match {
        case n1 ~ n2 => n1 + n2
      }) should be(3)

      val hyper = 1 ~ 2 ~ 3
      (hyper match {
        case HyperEdge(n1, n2, n3, _*) => n1 + n2 + n3
      }) should be(6)
      (hyper match {
        case n1 ~~ (n2, n3) => n1 + n2 + n3
      }) should be(6)

      val diHyper = 1 ~> 2 ~> 3
      (diHyper match {
        case DiHyperEdge(_, t1, _*) => t1
      }) should be(2)
      (diHyper match {
        case _ ~~> (t1, t2) => t1 + t2
      }) should be(5)
    }
  }
}
private object Helper {
  def incrementNode(p: Param[Int, UnDiEdge]): Param[Int, UnDiEdge] = p match {
    case out: OutParam[_, _] =>
      out match {
        case InnerNodeParam(n) => OuterNode(n + 1)
        case e: InnerEdgeParam[Int @unchecked, UnDiEdge @unchecked, _, _] =>
          val innerEdge = e.asEdgeTProjection[Int, UnDiEdge].edge
          UnDiEdge((innerEdge._1.value + 1, innerEdge._2.value + 1))
      }
    case _ => throw new IllegalArgumentException
  }
  def nodeToString(p: Param[Int, DiEdge]): Param[String, UnDiEdge] = p match {
    case in: InParam[_, DiEdge] => throw new IllegalArgumentException
    case out: OutParam[_, _] =>
      out match {
        case InnerNodeParam(n) => OuterNode(n.toString)
        case e: InnerEdgeParam[Int @unchecked, DiEdge @unchecked, _, _] =>
          val innerEdge = e.asEdgeTProjection[Int, DiEdge].edge
          UnDiEdge(innerEdge._1.value.toString, innerEdge._2.value.toString)
      }
  }
}
