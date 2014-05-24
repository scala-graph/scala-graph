package scalax.collection

import language.{higherKinds, postfixOps}
import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe._

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._
import generic.GraphCompanion
import config._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** This wrapper trait enables to transparently pass `GraphCompanion` objects with
 *  non-default configuration parameters to tests in a type-safe way. */
trait ConfigWrapper[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
{
  val companion: GraphCompanion[CC]
  implicit val config: companion.Config
  def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                      config: companion.Config): CC[N,E] = companion.empty
  def apply[N, E[X] <: EdgeLikeIn[X]](elems: Param[N,E]*)
                                     (implicit edgeT: TypeTag[E[N]],
                                      config: companion.Config) = companion(elems: _*)
  def from [N, E[X] <: EdgeLikeIn[X]](edges: collection.Iterable[E[N]])
                                     (implicit edgeT: TypeTag[E[N]],
                                      config: companion.Config) = companion.from(edges = edges)
  def from [N, E[X] <: EdgeLikeIn[X]](nodes: collection.Iterable[N],
                                      edges: collection.Iterable[E[N]])
                                     (implicit edgeT: TypeTag[E[N]],
                                      config: companion.Config) = companion.from(nodes, edges)
}
@RunWith(classOf[JUnitRunner])
class TEditRootTest
	extends Suites( 
			new TEdit[          Graph](new ConfigWrapper[          Graph]{
			                             val companion =           Graph
			                             val config    =           Graph.defaultConfig }),
			new TEdit[immutable.Graph](new ConfigWrapper[immutable.Graph]{
			                             val companion = immutable.Graph
			                             val config    = immutable.Graph.defaultConfig }),
			new TEdit[  mutable.Graph](new ConfigWrapper[  mutable.Graph]{
                                   val companion =   mutable.Graph
                                   val config    =   mutable.Graph.defaultConfig })
		)
	with ShouldMatchers
{
	// ---------------------------------------- immutable tests
	def test_DefaultIsImmutable {
		val g = Graph[Nothing,Nothing]()
		g.isInstanceOf[immutable.Graph[Nothing,Nothing]] should be (true)
	}
  def test_CanBuildFromImmutableUnDi {
    import immutable.Graph
    val g = Graph(1~2)
    val m: Graph[Int,UnDiEdge] = g map Helper.icrementNode
    m.edges.head should be (UnDiEdge(2,3))
  }
	// ---------------------------------------- mutable tests
	val mutableFactory = mutable.Graph 
  def test_PlusEq {
    val g = mutableFactory[Int,Nothing](1, 3)
    g += 2
    g should have size (3)
    for (i <- 1 to 3)
      g.contains(i) should be (true) //g should contain (i)
  }
  def test_MinusEq {
    val g = mutableFactory(1, 2, 2~3, 4)
    g remove 1 should be (true)
    g          should be (mutableFactory(2~3, 4))
    g remove 5 should be (false)
    g -?  2    should be (g)
    (g -?= 2)  should be (g)
    (g -= 2)   should be (mutableFactory[Int,UnDiEdge](3, 4))
    g.clear
    g          should be ('empty)
	}
  def test_MinusEq_2 {
    val g = mutableFactory(1~2, 2~3)
    (g -=  2)   should be (mutableFactory[Int,UnDiEdge](1, 3))
    g.graphSize should be (0)
  }
  def test_diSucc {
    val (one, two, oneOne, oneTwo) = (1, 2, 1~>1, 1~>2)
    val g = mutableFactory(oneOne, oneTwo, one~>3, one~>4)
    val (n1, n2) = (g get one, g get two)
    val e11 = g get oneOne
    
    g -= 1~>4 // Graph(oneOne, oneTwo, one~>3)
    (n2 diSuccessors) should be ('isEmpty)
    (n1 diSuccessors) should be (Set(two,3))
    (n1 ~>? n1) should be (Some(e11))
    
    g -= oneTwo // Graph(oneOne, one~>3)
    (n1 diSuccessors) should be (Set(3))
    (n1 ~>? n1) should be (Some(e11))
    
    g -= oneOne // Graph(one~>3)
    (n1 diSuccessors) should be (Set(3))
    (n1 ~>? n1) should be (None)
    
    g ++= List(oneOne, oneTwo) // Graph(oneOne, oneTwo, one~>3)
    (n1 diSuccessors) should be (Set(two,3))
    (n1 ~>? n1) should be (Some(e11))
  }
  def test_diSuccDiHyper {
    val (one, two, three, oneOneTwo, oneTwoThree) = (1, 2, 3, 1~>1~>2, 1~>2~>3)
    val g = mutableFactory(oneOneTwo, oneTwoThree)
    val (n1, n2) = (g get one, g get two)
    val e112 = g get oneOneTwo

    (n2 diSuccessors) should be ('isEmpty)
    (n1 diSuccessors) should be (Set(two, three))
    (n1 ~>? n1) should be (Some(oneOneTwo))
    
    g -= oneTwoThree // Graph(oneOneTwo)
    (n1 diSuccessors) should be (Set(two))
    (n1 ~>? n1) should be (Some(oneOneTwo))
    
    g -= two // Graph(one)
    (n1 diSuccessors) should be ('isEmpty)
    (n1 ~>? n1) should be (None)
    
    g += oneOneTwo // Graph(oneOneTwo)
    (n1 diSuccessors) should be (Set(2))
    (n1 ~>? n1) should be (Some(oneOneTwo))
  }
  def test_PlusEdgeEq {
    val g = mutableFactory(2~3)
    def n(i: Int) = g get i
    implicit val unDiFactory = UnDiEdge 
    g addEdge (n(3), n(2))  should be (false)
    (g +~=    (n(2), n(2))) should have size (4)

    g.addAndGetEdge(2, 3)(DiEdge).directed should be (true)
    g should have ('order (2), 'graphSize (3))

    n(3) +~ (4)
    g should have ('order (3), 'graphSize (4))

    (n(3) +~ n(2))(DiEdge)
    g should have ('order (3), 'graphSize (5))
  }
  def test_PlusHyperEdgeEq {
    implicit val factory = HyperEdge
    val h = mutableFactory(1~1~2)
    h should have ('order (2), 'graphSize (1))
    h +~= (0, 1, 2, 3)
    h should have ('order (4), 'graphSize (2))
  }
  def test_PlusWEdgeEq {
    val g = mutableFactory(2~3)
    implicit val f = edge.WUnDiEdge
    g.addWEdge (3,4)(2)
    g should have ('order (3), 'graphSize (2), 'totalWeight (3))
    (g +~%= (2,4))(3)
    g should have ('order (3), 'graphSize (3), 'totalWeight (6))
    // (g +~%= (0,1,2,3))(3)(edge.WHyperEdge) // must not compile
  }
  def test_PlusWHyperEdgeEq {
    implicit val factory = edge.WHyperEdge
    val h = mutableFactory(1~1~2)
    h should have ('order (2), 'graphSize (1))
    h.addWEdge (3,4,5)(2)
    h should have ('order (5), 'graphSize (2), 'totalWeight (3))
    (h +~%= (0,1,2,3))(3)
    h should have ('order (6), 'graphSize (3), 'totalWeight (6))
  }
  def test_PlusLEdgeEq {
    import edge.Implicits._
    import edge.{LUnDiEdge, LDiEdge}

    type StringLabel = Option[String]
    val str = "A"
    val label: StringLabel = Some(str)
    val g = mutableFactory(2~3, (2 ~+# 3)(label))
    g should have ('order (2), 'graphSize (2))

    import edge.LBase.{LEdgeImplicits}
    object StringLabelImplicit extends LEdgeImplicits[StringLabel]
    import StringLabelImplicit._
    for (e <- g.edges if e.isLabeled) {
      e.isDefined should be (true)
      e.get       should be (str)
    }

    type ListLabel = List[Int]
    object ListLabelImplicit extends LEdgeImplicits[ListLabel]
    import ListLabelImplicit._
    implicit val factory = LDiEdge
    val listLabel = List(1,0,1)
    g.addLEdge(3,4)(listLabel) should be (true)
    g should have ('order (3), 'graphSize (3))
    val findAdded = g.edges find (3~>4)
    findAdded should be ('isDefined)
    val added: g.EdgeT = findAdded.get
    added.directed should be (true)
    added.count(_ > 0) should be (List(1,0,1).count(_ > 0))
  }
  def test_PlusLHyperEdgeEq {
    import edge.Implicits._
    import edge.{LHyperEdge, LDiHyperEdge}

    type StringLabel = String
    val outerLabels = Seq("A", "BC", "CDE")
    val g = mutableFactory(1~2~3, (2 ~+# 3)(outerLabels(0)))

    implicit val factory = LHyperEdge
    (g +~+= (3,4,5))(outerLabels(1))
    g should have ('order (5), 'graphSize (3))
    g.addLEdge(4,5,6)(outerLabels(2)) should be (true)
    g should have ('order (6), 'graphSize (4))

    import edge.LBase.{LEdgeImplicits}
    object StringLabelImplicit extends LEdgeImplicits[StringLabel]
    import StringLabelImplicit._
    val innerLabels: collection.mutable.Set[_ >: StringLabel] =
      g.edges filter (_.isLabeled) map (_.label)
    innerLabels should have size (outerLabels.size)
    /* 
    innerLabels forall (outerLabels contains _) should be (true) 
     * https://groups.google.com/forum/?fromgroups=#!searchin/scala-internals/both$20method/scala-internals/nPZY2EMtDvY/PivCCtyRM_IJ
     * https://issues.scala-lang.org/browse/SI-5330 
     */
    (innerLabels: Iterable[Any]) forall (outerLabels contains _) should be (true) 
  }
  def test_pluPlusEq {
    val (gBefore, gAfter) = (mutableFactory(1, 2~3), mutableFactory(0, 1~2, 2~3)) 
    (gBefore ++= List[Param[Int,UnDiEdge]](1~2, 2~3, 0)) should equal (gAfter)
    (gBefore ++= mutableFactory(0, 1~2))                      should equal (gAfter)
    (gBefore ++= mutableFactory[Int,UnDiEdge](0) ++= mutableFactory(1~2))   should equal (gAfter)
  }
  def test_upsert {
    import edge.LDiEdge, edge.LBase._
    val (label, modLabel) = ("A", "B")
    val g = mutableFactory(LDiEdge(1, 2)(label), LDiEdge(2, 3)(label))

    g.edges foreach { _.edge match {
      case LDiEdge(s, t, l) => g upsert (LDiEdge(s.value, t.value)(modLabel)) 
    }}
    g should have ('graphSize (2))
    g.edges foreach { _.label  should be (modLabel)
    }
  }
  def test_CanBuildFromMutableUnDi {
    import mutable.Graph
    val g = Graph(1~2)
    val m: Graph[Int,UnDiEdge] = g map Helper.icrementNode
    m.edges.head should be (UnDiEdge(2,3))
  }
}
/**	Contains tests for graph editing to be run for Graph instances created by `factory`.
 *  For instance, this allows the same tests to be run for mutable and immutable Graphs.
 *  Additionally it is possible to pass different configurations.
 */
class TEdit[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: ConfigWrapper[CC])
	extends	Spec
	with	ShouldMatchers
{
  implicit val config = factory.config

	val seq_1_3	 = Seq(1, 3)
	val gInt_1_3 = factory[Int,DiEdge](seq_1_3: _*)
	val gString_A = factory[String,Nothing]("A")

	def test_0(info : Informer) {
		info("factory = " + factory.companion.getClass)
	}
	def test_Empty {
		val eg = factory.empty[Nothing,Nothing] 
		eg should be ('isEmpty)
		eg should have size (0)
		eg should equal (eg.empty)
	}
	def test_Factory {
		gInt_1_3 should not be ('isEmpty)
		gInt_1_3 should have size (2)
		gInt_1_3(0) should be (false)
		gInt_1_3(1) should be (true)
		gInt_1_3(2) should be (false)
		gInt_1_3(3) should be (true)

		val r = HyperEdge(1, 2, 3)
		val g = factory(1, r, 1~2)
		g.nodes	should have size (3)
		g.edges	should have size (2)
		g			  should have size (5)
		g.contains(r) should be (true)
		
		val h = factory(UnDiEdge(1, 2), UnDiEdge(2, 3))
		h.nodes	should have size (3)
		h.edges	should have size (2)
		h	  		should have size (5)
	}
	def test_isDirected {
	  factory(1~2).isDirected should be (false)
	  factory(edge.WDiEdge(1, 2)(0)).isDirected should be (true)
	}
  def test_isHyper {
    factory(1~2).isHyper should be (false)
    factory(edge.WDiHyperEdge(1, 2, 3)(0)).isHyper should be (true)
  }
	def test_Constructor {
	  val (n_start, n_end) = (11, 20)
	  val nodes = List.range(n_start, n_end)
	  val edges = List[DiEdge[Int]](14~>16, 16~>18, 18~>20, 20~>22)
	  val g = factory.from(nodes, edges)
	  g.nodes.size should be (nodes.size + 2)
	  g.edges.size should be (edges.size)
	}
	def test_Contain {
		seq_1_3 foreach {
			i => gInt_1_3.contains(i) should be (true) //gInt_1_3 should contain (i)
		}
		gInt_1_3.head.isInstanceOf[InnerNodeParam[Int]] should be (true)
	}
	def test_toString {
	  val nodePrefix = OuterNode.stringPrefix
		gInt_1_3 .toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """[13], """ + nodePrefix + """[13]\)""")
		gString_A.toString should fullyMatch regex ("""Graph\(""" + nodePrefix + """["A"]\)""")
	}
	def test_fromInner {
	  val gn = factory[Int,UnDiEdge](2,3)
    factory(gn.nodes: _*)                    should equal (gn)
    factory.from[Int,Nothing](gn.nodes, Nil) should equal (gn)
	  
    val g = factory(2~3)
    factory(g.edges.head) should equal (g)
    factory(g.edges: _*)  should equal (g)
    factory.from[Int,UnDiEdge](edges = g.edges) should equal (g)
	}
	def test_plusInt {
	  val g = factory(1, 2~3)
	  g + 1   should be (g)
	  g + 0   should be (Graph(0, 1, 2, 3, 2~3))
	  g + 0~1 should be (Graph(0, 1, 2, 3, 0~1, 2~3))
	  //g + "A" !!! // error: type mismatch
	}
	def test_PlusString {
		val g = gString_A + "B"
		g should have size (2)
		g.contains("A") should be (true) //g should contain ("A")
		g.contains("B") should be (true) //g should contain ("B")

		val hString_A = factory[String, UnDiEdge]("A")
		val h = hString_A + ("A"~"C")
		h.nodes		should have size (2)
		h.edges	should have size (1)
		h			should have size (3)
	}
	def test_PlusPlus {
		val g = gString_A + "B" + "C"
		g should have size (3)
		g.contains("A") should be (true) //g should contain ("A")
		g.contains("B") should be (true) //g should contain ("B")
		g.contains("C") should be (true) //g should contain ("C")

		val (gBefore, gAfter) = (factory(1, 2~3), factory(0, 1~2, 2~3)) 
		gBefore ++ List[Param[Int,UnDiEdge]](1~2, 2~3, 0) should equal (gAfter)
		gBefore ++ factory(0, 1~2)                             should equal (gAfter)
    gBefore ++ factory[Int,UnDiEdge](0) ++ factory(1~2)    should equal (gAfter)
	}
	def test_Minus {
		var g = gString_A - "B"
		g should have size (1)

		g = gString_A - "A"
		g.contains("A") should be (false) //gMinus should not contain ("A")
		g should have size (0)
		g should be ('isEmpty)

		val h = factory(1, 2, 2~3)
		h  - 0 should be (h)
		h  - 1 should be (factory(2, 2~3))
    h -? 2 should be (h)
    h  - 2 should be (factory[Int,UnDiEdge](1, 3))
	}
	def test_MinusMinus {
    val g = factory(1, 2~3, 3~4)
	  g --  List[Param[Int,UnDiEdge]](2, 3~3) should be (factory(1, 3~4))
    g --  List[Param[Int,UnDiEdge]](2, 3~4) should be (factory[Int,UnDiEdge](1, 3, 4))
	  g --! List[Param[Int,UnDiEdge]](1, 3~4) should be (factory(2~3))
	}
  def test_CanBuildFromUnDi {
    val g = factory(0, 1~2)
    // TODO CC[Int,UnDiEdge]
  	val m: Graph[Int,UnDiEdge] = g map Helper.icrementNode
    m find 1 should be ('defined)
    m.edges.head should be (UnDiEdge(2,3))
  }
  def test_CanBuildFromDi {
    val g = factory(1~>2)
    val m: Graph[String,UnDiEdge] = g map Helper.nodeToString
    m.edges.head should be ("1"~"2")
  }
	def test_NodeSet {
	  val o = Array.range(0, 4)
    val g = factory(o(1) ~ o(2), o(2) ~ o(3))
    val n = o map (g.nodes find _ getOrElse g.nodes.head)
	  
    val less = g.nodes - n(3)
    less should have size (2)
    less should contain (n(1))
	  less.find(_ == n(1)).get.edges should have size (1)
    less should contain (n(2))
    less.find(_ == n(2)).get.edges should have size (2)

	  val restored = less + n(3)
	  restored should have size (3)
    restored should contain (n(3))
    restored.find(_ == n(1)).get.edges should have size (1)
}
	def test_Eq {
		factory[Int,Nothing]() shouldEqual factory[Int,Nothing]()
		gInt_1_3	shouldEqual factory[Int,DiEdge](seq_1_3: _*)
		gString_A	shouldEqual factory[String,Nothing]("A")

		factory[Int,Nothing]()	should not be (factory[Int,Nothing](1))
		gInt_1_3	should not be (factory[Int,DiEdge](2, 3))
		gString_A	should not be (factory[String,Nothing]("B"))

		gInt_1_3	should be (factory[Int,DiEdge](1) + 3) 
	}
	def test_EdgeAssoc {
    val e = 1 ~ 2 
    e.isInstanceOf[UnDiEdge[Int]] should be (true)
    val x = factory(3~4).nodes
    // Error in Scala compiler: assertion failed
    // Graph(3).nodes contains 3 //should be (true)  

    val d = 1 ~> 2 
    d.isInstanceOf[DiEdge[Int]] should be (true)
    d.source should be (1)
    d.target should be (2)
    factory(1, d, 1~4).nodes should have size (3)

    val heNodes = List("A", "B", "C")
    val he = heNodes(0) ~ heNodes(1) ~ heNodes(2)
    he.isInstanceOf[HyperEdge[String]] should be (true)
    he.arity should be (heNodes.size)
    he._1    should be (heNodes(0))  
    he._2    should be (heNodes(1))
    for (i <- 0 to (heNodes.size - 1))
      he._n(i) should be (heNodes(i))  

    val dhe = "A" ~> "B" ~> "C" ~> "D"
    dhe.isInstanceOf[DiHyperEdge[String]] should be (true)
    dhe.arity should be (4)
    dhe._1    should be ("A")  
    dhe._n(3) should be ("D")
	}
  def test_diSuccessorsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 diSuccessors) should be (Set(2, 3, 4))
    (g get 2 diSuccessors) should be (Set(1))
  }
  def test_diSuccessorsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 diSuccessors) should be (Set(2, 3, 4))
    (g get 2 diSuccessors) should be (Set.empty)
  }
  def test_diSuccessorsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 diSuccessors) should be (Set(2, 3, 4, 5, 9))
    (h get 2 diSuccessors) should be (Set.empty)
    (h get 5 diSuccessors) should be (Set.empty)
  }
  def test_diPredecessorsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 diPredecessors) should be (Set(2, 3, 4))
    (g get 2 diPredecessors) should be (Set(1))
  }
  def test_diPredecessorsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 diPredecessors) should be (Set.empty)
    (g get 2 diPredecessors) should be (Set(1))
  }
  def test_diPredecessorsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 diPredecessors) should be (Set.empty)
    (h get 2 diPredecessors) should be (Set(1))
    (h get 5 diPredecessors) should be (Set(1))
  }
  def test_neighborsUnDi {
    val g = factory (1~1, 1~2, 1~3, 1~4)
    (g get 1 neighbors) should be (Set(2, 3, 4))
    (g get 2 neighbors) should be (Set(1))
  }
  def test_neighborsDi {
    val g = factory (1~>1, 1~>2, 1~>3, 1~>4)
    (g get 1 neighbors) should be (Set(2,3,4))
    (g get 2 neighbors) should be (Set(1))
  }
  def test_neighborsDiHyper {
    val h = factory (1~>1~>5, 1~>2~>5, 1~>3~>5, 1~>4~>9)
    (h get 1 neighbors) should be (Set(2,3,4,5,9))
    (h get 2 neighbors) should be (Set(1,5))
    (h get 5 neighbors) should be (Set(1,2,3))
  }
  def test_findOutgoingToDi {
    val g = factory (1~>1, 1~>2, 2~>1)
    def n(i: Int) = g get i 
    (n(1) findOutgoingTo n(2)) should be (Some(1~>2))
    (n(1) findOutgoingTo n(1)) should be (Some(1~>1))
  }
	def test_degree {
	  val g = factory (1~1, 1~2, 1~3, 1~4 )
	  (g get 1 degree) should be (5)
    (g get 2 degree) should be (1)
	}
  def test_incoming {
    val uEdges = Seq[UnDiEdge[Int]](1~1, 1~2, 1~3, 1~4) // bug if no type param given
    val g = factory (uEdges(0), uEdges(1), uEdges(2), uEdges(3))
    (g get 1 incoming) should be (uEdges.toSet)
    (g get 2 incoming) should be (Set(uEdges(1)))
    
    val dEdges = Seq[DiEdge[Int]](1~>1, 1~>2, 1~>3, 1~>4)
    val h = factory (dEdges(0), dEdges(1), dEdges(2), dEdges(3))
    (h get 1 incoming) should be (Set(dEdges(0)))
    (h get 2 incoming) should be (Set(dEdges(1)))
  }
  def test_edgeAdjacents_UnDi_1 {
    val g = Graph(1~2, 2~3, 1~>3, 1~5, 3~5, 3~4, 4~>4, 4~>5)
    ((g get 4~>4) adjacents) should be (Set(3~4, 4~>5))
    ((g get 1~2)  adjacents) should be (Set(1~>3, 1~5, 2~3))
  }
  def test_predicate {
    val g = factory(2~>3, 3~>1, 5)
    g  filter ((n: Int) => n > 1) should be (factory(2~>3, 5))
    g  filter ((n: Int) => n < 2) should be (factory[Int,DiEdge](1))
    g  filter g.having(node = _ <  2) should be (factory[Int,DiEdge](1))
    g  filter g.having(node = _ >= 2) should be (factory(2~>3, 5))
    g  filter g.having(edge = _._1 == 2)    should be (factory(2~>3))
    g  filter g.having(edge = _ contains 2) should be (factory(2~>3))
  }
  def test_match {
    val di = 1 ~> 2
    (di match { case DiEdge(src, _) => src }) should be (1)
    (di match { case src ~> trg => src + trg }) should be (3)
    
    val unDi = 1 ~ 2
    (unDi match { case UnDiEdge(n1, _) => n1 }) should be (1)
    (unDi match { case n1 ~ n2 => n1 + n2 }) should be (3)
    
    val hyper = 1 ~ 2 ~ 3
    (hyper match { case HyperEdge(n1, n2, n3, _*) => n1 + n2 + n3 }) should be (6)
    (hyper match { case n1 ~~ (n2, n3) => n1 + n2 + n3 }) should be (6)
    
    val diHyper = 1 ~> 2 ~> 3
    (diHyper match { case DiHyperEdge(_, t1, _*) => t1 }) should be (2)
    (diHyper match { case _ ~~> (t1, t2) => t1 + t2 }) should be (5)
  }
}
object Helper {
  def icrementNode(p: Param[Int,UnDiEdge]): Param[Int,UnDiEdge] = p match {
    case in: InParam[_,UnDiEdge] => throw new IllegalArgumentException 
    case out: OutParam[_,_] => out match {
      case n: InnerNodeParam[Int] => OuterNode(n.value + 1)
      case o: InnerEdgeParam[Int,UnDiEdge,_,UnDiEdge] =>
        val e = o.asInstanceOf[Graph[Int,UnDiEdge]#EdgeT]
        UnDiEdge((e.edge._1.value + 1, e.edge._2.value + 1)) 
    } 
  }
  def nodeToString(p: Param[Int,DiEdge]): Param[String,UnDiEdge] = p match {
    case in: InParam[_,DiEdge] => throw new IllegalArgumentException 
    case out: OutParam[_,_] => out match {
      case n: InnerNodeParam[Int] => OuterNode(n.value.toString)
      case o: InnerEdgeParam[Int,DiEdge,_,DiEdge] =>
        val e = o.asInstanceOf[Graph[Int,DiEdge]#EdgeT]
        UnDiEdge(e.edge._1.value.toString, e.edge._2.value.toString) 
    } 
  }
}