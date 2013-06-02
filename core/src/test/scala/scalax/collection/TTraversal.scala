package scalax.collection

import org.scalatest.Suite
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{ListBuffer, Set, Stack}

import GraphPredef._, GraphEdge._
import GraphTraversal.VisitorReturn._
import generic.GraphCoreCompanion
import edge.WDiEdge, edge.WUnDiEdge, edge.Implicits._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TTraversalRootTest
	extends Suites( 
			new TTraversal[immutable.Graph](immutable.Graph),
			new TTraversal[  mutable.Graph](  mutable.Graph)
		)
	with ShouldMatchers
{
}
/**	This class contains tests for graph traversals to be run for Graph instances created
 *	by the Graph factory and passed to the constructor. For instance,
 *	this allows the same tests to be run for mutable and immutable Graphs.
 */
private class TTraversal[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
			(val factory: GraphCoreCompanion[CC])
	extends	Suite
	with	ShouldMatchers
{
  def test_findSuccessor_tiny {
    val g = factory(1~>2)
    val (n1, n2) = (g get 1, g get 2)

    var successor = n1 findSuccessor (_ == 1)
    successor should be ('isEmpty)

    successor = n1 findSuccessor (_ == 3)
    successor should be ('isEmpty)
    
    successor = n2 findSuccessor (_ == 1)
    successor should be ('isEmpty)
    
    successor = n1 findSuccessor (_ == 2)
    successor     should be ('isDefined)
    successor.get should be (n2)
  }
  def test_findPredecessor_tiny {
    val g = factory(1~>2)
    val (n1, n2) = (g get 1, g get 2)

    var predecessor = n1 findPredecessor (_ == 1)
    predecessor should be ('isEmpty)

    predecessor = n1 findPredecessor (_ == 3)
    predecessor should be ('isEmpty)
    
    predecessor = n1 findPredecessor (_ == 2)
    predecessor should be ('isEmpty)
    
    predecessor = n2 findPredecessor (_ == 1)
    predecessor     should be ('isDefined)
    predecessor.get should be (n1)
  }
  def test_findConnected_tiny {
    val g = factory(1~>2)
    val (n1, n2) = (g get 1, g get 2)

    var predecessor = n1 findConnected (_ == 1)
    predecessor should be ('isEmpty)

    predecessor = n1 findConnected (_ == 3)
    predecessor should be ('isEmpty)
    
    predecessor = n1 findConnected (_ == 2)
    predecessor     should be ('isDefined)
    predecessor.get should be (n2)
    
    predecessor = n2 findConnected (_ == 1)
    predecessor     should be ('isDefined)
    predecessor.get should be (n1)
  }
  import Data._
  object Di_1 extends TGraph[Int, DiEdge](factory(elementsOfDi_1: _*))
  def test_findSuccessor_mid {
    val g = Di_1
    def n(outer: Int) = g.node(outer)
    var successor = null.asInstanceOf[Option[g.g.NodeT]]

    successor = n(3) findSuccessor (_ == 0)
    successor should be ('isEmpty)

    successor = n(3) findSuccessor (_ == 3)
    successor should be ('isEmpty)

    successor = n(3) findSuccessor (_ == 7)
    successor should be ('isEmpty)

    successor = n(2) findSuccessor (_ == 5)
    successor     should be ('isDefined)
    successor.get should be (5)

    successor = n(3) findSuccessor (_ > 4)
    successor     should be ('isDefined)
    successor.get should be (5)
  }
  def test_findPredecessor_mid {
    val g = Di_1
    def n(outer: Int) = g.node(outer)
    var predecessor = null.asInstanceOf[Option[g.g.NodeT]]

    predecessor = n(3) findPredecessor (_ == 0)
    predecessor should be ('isEmpty)

    predecessor = n(3) findPredecessor (_ == 3)
    predecessor should be ('isEmpty)

    predecessor = n(3) findPredecessor (_ == 5)
    predecessor should be ('isEmpty)

    predecessor = n(3) findPredecessor (_ == 4)
    predecessor     should be ('isDefined)
    predecessor.get should be (4)

    predecessor = n(3) findPredecessor (_ > 2)
    predecessor     should be ('isDefined)
    predecessor.get should be (4)
  }
  def test_findConnected_mid {
    val g = Di_1
    def n(outer: Int) = g.node(outer)
    var connected = null.asInstanceOf[Option[g.g.NodeT]]

    connected = n(3) findConnected (_ == 0)
    connected should be ('isEmpty)

    connected = n(3) findConnected (_ == 3)
    connected should be ('isEmpty)

    connected = n(2) findConnected (_ == 4)
    connected     should be ('isDefined)
    connected.get should be (4)

    connected = n(3) findConnected (_ > 3)
    connected     should be ('isDefined)
    connected.get should (be (4) or be (5))
  }
  def test_findPathToSuccessor_tiny {
    val g = factory(1, 2~3, 3~4, 5~6, 6~1)

    val n1 = g get 1
    n1 pathUntil (_ == n1) should be (None)

    val n2 = g get 2 
    val p2 = n2 pathUntil (_ == n1) should be (None) 

    val n5 = g get 5
    val n6 = g get 6
    val expected = List(n5, n6, n1)
    val r5 = n5 pathUntil (_ < 4)
    r5     should be ('isDefined)
    val p5 = r5.get
    p5.nodes should be (expected) 

    p5.size   should be (expected.size + (expected.size - 1))
    p5.length should be (expected.size - 1)
  }
  def test_shortestPathTo_fix_110409 {
    val g = factory(0~1, 1~2, 2~3)
    def n(outer: Int) = g get outer
    (n(0) shortestPathTo n(3)).get.nodes should be (List(0,1,2,3))
    (n(1) shortestPathTo n(3)).get.nodes should be (List(  1,2,3))
  }
  def test_shortestPathTo_Di_1 {
    val g = factory(elementsOfWDi_1: _*)
    def n(outer: Int) = g get outer

    n(5) shortestPathTo n(4) should be (None)
    n(5) shortestPathTo n(1) should be (None)
    n(3) shortestPathTo n(1) should be (None)
    
    (n(1) shortestPathTo n(3)).get.nodes should be (List(1,3)) 
    (n(4) shortestPathTo n(5)).get.nodes should be (List(4,3,5)) 
    (n(1) shortestPathTo n(5)).get.nodes should be (List(1,5))
  }
  def test_shortestPathTo_UnDi_1 {
    val g = factory(elementsofWUnDi_1: _*)
    def n(value: Int) = g get value

    (n(2) shortestPathTo n(5)).get.nodes should be (List(2,3,4,5))
    (n(4) shortestPathTo n(5)).get.nodes should be (List(4,5))
    (n(1) shortestPathTo n(3)).get.nodes should(be (List(1,3)) or be (List(1,5,3)))
    (n(5) shortestPathTo n(4)).get.nodes should be (List(5,3,4))
    (n(3) shortestPathTo n(1)).get.nodes should be (List(3,4,5,1))
  }
  // see diagram WUnDi-2.jpg
  val eUnDi_2 = List[WUnDiEdge[Int]](
                1~2 % 4, 2~3 % -1, 1~>3 % 5, 1~3 % 4, 1~>2 % 3, 2~2 % 1)
             // 0        1         2         3        4         5
  val gUnDi_2 = factory.from[Int,WUnDiEdge](Set.empty, eUnDi_2)
  def test_shortestPathTo_UnDi_2 {
    def n(value: Int) = gUnDi_2 get value

    val p1_3 = n(1).shortestPathTo(n(3)).get
    p1_3.nodes should be (List(1,2,3))
    p1_3.edges should be (List(eUnDi_2(4), eUnDi_2(1)))

    val p2_1 = (n(2) shortestPathTo n(1)).get
    p2_1.nodes should be (List(2,3,1))
    p2_1.edges should be (List(eUnDi_2(1), eUnDi_2(3)))

    val p3_1 = (n(3) shortestPathTo n(1)).get
    p3_1.nodes should be (List(3,2,1))
    p3_1.edges should be (List(eUnDi_2(1), eUnDi_2(0)))

    val p3_3 = (n(3) shortestPathTo n(3)).get
    p3_3.nodes should be (List(3))
    p3_3.edges should be ('empty)
  }
  def test_Filter {
    def n(value: Int) = gUnDi_2 get value

    val p2_1_nNE3 = (n(2) pathTo (n(1),
                                  nodeFilter = _ != 3)).get
    p2_1_nNE3.nodes should be (List(2,1))
    p2_1_nNE3.edges should be (List(2~1 % 4))

    val p1_3_wGT4 = (n(1) pathTo (n(3),
                                  edgeFilter = _.weight > 4)).get
    p1_3_wGT4.nodes should be (List(1,3))
    p1_3_wGT4.edges should be (List(eUnDi_2(2)))

    val p1_3_wLT4 = (n(1) pathTo (n(3),
                                  edgeFilter = _.weight < 4)).get
    p1_3_wLT4.nodes should be (List(1,2,3))
    p1_3_wLT4.edges should be (List(eUnDi_2(4),eUnDi_2(1)))
}
  def test_Visitor {
    def n(value: Int) = gUnDi_2 get value

    var nodes = ListBuffer[gUnDi_2.NodeT]()
    var edges = ListBuffer[gUnDi_2.EdgeT]()
    val p2_1_nNE3 = (n(2) pathTo (n(1),
                                  nodeFilter  = _ != 3,
                                  nodeVisitor = (n: gUnDi_2.NodeT) => {nodes += n; Continue},
                                  edgeVisitor = (e: gUnDi_2.EdgeT) => {edges += e} )).get
    nodes should be (List(n(2), n(1)))
    edges.toList.sorted(gUnDi_2.Edge.WeightOrdering) should be (List(eUnDi_2(1), eUnDi_2(5), eUnDi_2(0)))
  }
  def test_ExtendedVisitor {
    def n(outer: Int) = Di_1.node(outer)
    
    import Di_1.g.ExtendedNodeVisitor
    import GraphTraversalImpl._

    n(1) pathTo (n(3),
                 nodeVisitor = ExtendedNodeVisitor((node, informer) => {
      informer match {
        case DfsInformer(depth) => depth should be >= (0)
        case _ => fail
      }
      Continue
    }))
  }
  def test_shortestPathFunctional {
    import custom.flight._, custom.flight.Helper._, custom.flight.FlightImplicits._
    val (jfc, lhr, dme, svx, fra, prg) = (
        Airport("JFC"), Airport("LHR"), Airport("DME"),
        Airport("SVX"), Airport("FRA"), Airport("PRG"))
    val flights: List[Flight[Airport]] =
      List(jfc ~> dme ## ("UN 2222", 14 o 25, 8 h 50),
           dme ~> svx ## ("UN 109" , 23 o 10, 2 h 15),
           jfc ~> lhr ## ("BA 174" , 19 o 10, 6 h 50),
           jfc ~> fra ## ("LH 400" , 10 o 25, 8 h 20),
           jfc ~> fra ## ("UA 8840", 15 o 40, 7 h 35),
           lhr ~> dme ## ("BA 872" ,  8 o 55, 4 h 00),
           lhr ~> dme ## ("SU 242" , 20 o 15, 3 h 50),
           lhr ~> fra ## ("LH 903" ,  9 o 50, 1 h 35),
           lhr ~> prg ## ("BA 860" , 11 o 15, 2 h 00),
           fra ~> lhr ## ("LH 920" , 19 o 50, 1 h 35),
           fra ~> dme ## ("LH 1444",  7 o 50, 3 h 10),
           fra ~> svx ## ("LH 1480", 19 o 20, 4 h 35),
           prg ~> svx ## ("U6 902" , 21 o 55, 4 h 25))
    def flight(flightNo: String) = flights find (_.flightNo == flightNo) get
    val g = factory.from[Airport, Flight](Set.empty, flights)

    val shp1 = (g get jfc) shortestPathTo (g get dme,
                                           edgeFilter = _.airline != "UN")
    shp1.get.nodes should be (List(jfc, lhr, dme))
    shp1.get.edges should be (List(flight("BA 174"),flight("SU 242")))
     
    val shp2 = (g get lhr) shortestPathTo (g get svx,
                                           edgeFilter = _.airline != "SU")
    shp2.get.edges should be (List(flight("LH 903"),flight("LH 1480")))

    val shp3 = (g get dme) shortestPathTo (g get jfc,
                                           nodeFilter = _ != fra)
    shp3 should be (None)
      
    val shp4 = (g get jfc) shortestPathTo (g get svx,
                                           nodeFilter = _ != dme)
    shp4.get.nodes should be (List(jfc, fra, svx)) 
    shp4.get.edges should be (List(flight("UA 8840"), flight("LH 1480")))

    var visited = Set[g.EdgeT]() 
    (g get jfc) shortestPathTo (g get lhr,
                                edgeVisitor = (e: g.EdgeT) => { visited += e })
    val visitedSorted = visited.toList.sortWith((a: g.EdgeT, b: g.EdgeT) => a.flightNo < b.flightNo)
    visitedSorted.sameElements(
        List(flight("BA 174"),
             flight("LH 400"),
             flight("UA 8840"),
             flight("UN 2222"))) should be (true) 
  }
  def test_Traversal {
    import Data._
    object UnDi_1 extends TGraph[Int, UnDiEdge](factory(elementsOfUnDi_1: _*)) {
      val expectedSumAll    = 15
      val expectedSumLayer1 = 12
      val expectedSumLayer2 = 15
      val expectedSumAllExclGt4    = 10
      val expectedSumLayer2ExclGt4 = 9
    }
    { import UnDi_1._
      var sum = 0
      def add(visited: g.NodeT) = { sum += visited; Continue }

      sum = 0
      node(4).traverseNodes() { add(_) }
      sum should be (expectedSumAll)

      sum = 0
      node(4).traverseNodes(maxDepth = 1) { add }
      sum should be (expectedSumLayer1)

      sum = 0
      node(4).traverseNodes(maxDepth = 2) { add }
      sum should be (expectedSumLayer2)

      sum = 0
      node(4).traverseNodes(breadthFirst = false, maxDepth = 1) { add }
      sum should be (expectedSumLayer1)

      sum = 0
      node(4).traverseNodes(breadthFirst = false, maxDepth = 2) { add }
      sum should be (expectedSumLayer2)

      sum = 0
      node(4).traverseNodes(nodeFilter = _ <= 4) { add }
      sum should be (expectedSumAllExclGt4)

      sum = 0
      node(4).traverseNodes(nodeFilter = _ <= 4, maxDepth = 2) { add }
      sum should be (expectedSumLayer2ExclGt4)
      
      val traversal = g.newTraversal(nodeFilter  = _ <= 4,
                                     nodeVisitor = add)
      sum = 0
      traversal bfs node(4)
      sum should be (expectedSumAllExclGt4)
      
      sum = 0
      traversal (node(4), maxDepth = 2)
      sum should be (expectedSumLayer2ExclGt4)

      sum = 0
      traversal dfs node(4)
      sum should be (expectedSumAllExclGt4)
    }
  }
  def test_DownUp {
    val g = Di_1.g
    def innerNode(outer: Int) = g get outer
    val stack: Stack[Int] = Stack()
    def down(n: g.NodeT) = { stack.push(n.value); Continue }
    def up  (n: g.NodeT) = { stack.pop should be (n.value) }
    
    innerNode(4).traverseDownUp()(down, up)
    stack should be ('empty)
  }
  def test_DownUpBraces {
    val root = "A"
    val g = factory(root~>"B1", root~>"B2")
    val result = ListBuffer.empty[String]
    (g get root).traverseDownUp()(
      nodeDown = (n: g.NodeT) => {
        result += (if (n == root) "(" else "[") += n
        Continue
      },
      nodeUp   = (n: g.NodeT) =>
        result += (if (n == root) ")" else "]")
    )
    ("" /: result)(_+_) should be ("(A[B1][B2])")
  }
  def test_DownUpSums {
    abstract class Elem(val name: String) {
      def balance: Int
    }
    case class Node(override val name: String) extends Elem(name) {
      var sum: Int = 0
      def balance = sum
    } 
    case class Leaf(override val name: String, override val balance: Int) extends Elem(name)
    val root = Node("R")
    val (nA, nB, nBA) = (Node("A"), Node("B"), Node("BA"))
    val g = factory[Elem,DiEdge](
        root ~> nA, root ~> nB,
        nA ~> Leaf("LA1",1), nA ~> Leaf("LA2",2),
        nB ~> Leaf("B1" ,3), nB ~> nBA,
        nBA~> Leaf("BA1",10),nBA~> Leaf("BA2",11), nBA~> Leaf("BA3",12))
    (g get root).traverseDownUp()(
      nodeDown = g.noNodeAction,
      nodeUp   = (node: g.NodeT) => node.value match {
        case n: Node => n.sum = (0 /: node.diSuccessors)(_ + _.balance)
        case _ =>
      }
    )
    val expected = Map(root->39, nA->3, nB->36, nBA->33)
    g.nodes foreach { _.value match {
      case n: Node => n.balance should be (expected(n))
      case _ =>
    }}
  }
  def test_TraversalDirection {
    import scalax.collection.GraphTraversal._
    // https://groups.google.com/forum/?fromgroups=#!topic/scala-internals/9NMPfU4xdhU
    object DDi_1 extends TGraph[Int, DiEdge](factory(elementsOfDi_1: _*)) {
      val expectedSumSuccessorsOf_4   = 12
      val expectedSumPredecessorsOf_4 = 4
      val expectedSumSuccessorsOf_2   = 10
      val expectedSumPredecessorsOf_2 = 3
      val expectedSumAnyConnected     = 15

      val expectedSumLayer1SuccessorsOf_2    = 5
      val expectedSumLayer1PredecessorsOf_2  = 3
      val expectedSumLayer1AnyConnectedsOf_2 = 6
    }
    { import DDi_1._
      var sum = 0
      def add(visited: g.NodeT) = { sum += visited; Continue }

      sum = 0
      node(4).traverseNodes(Successors) { add }
      sum should be (expectedSumSuccessorsOf_4)

      sum = 0
      node(4).traverseNodes(Predecessors) { add }
      sum should be (expectedSumPredecessorsOf_4)

      sum = 0
      node(2).traverseNodes(Successors) { add }
      sum should be (expectedSumSuccessorsOf_2)

      sum = 0
      node(2).traverseNodes(Predecessors) { add }
      sum should be (expectedSumPredecessorsOf_2)
      
      sum = 0
      node(2).traverseNodes(AnyConnected) { add }
      sum should be (expectedSumAnyConnected)

      sum = 0
      node(2).traverseNodes(Successors, maxDepth = 1) { add }
      sum should be (expectedSumLayer1SuccessorsOf_2)

      sum = 0
      node(2).traverseNodes(Predecessors, maxDepth = 1) { add }
      sum should be (expectedSumLayer1PredecessorsOf_2)
      
      sum = 0
      node(2).traverseNodes(AnyConnected, maxDepth = 1) { add }
      sum should be (expectedSumLayer1AnyConnectedsOf_2)
    }
  }
  def test_NodeOrdering {
    val root = 0
    val g = factory(0~>4, 0~>2, 0~>3, 0~>1,
                    1~>13, 1~>11, 1~>12,
                    2~>22, 2~>21, 2~>23,
                    3~>32, 3~>33, 3~>31,
                    4~>42, 4~>41, 4~>43)
    val resultOrder = new Array[Int](g.order)
    val nodeOrdering = g.NodeOrdering(Ordering.Int.compare(_,_))
    var i = 0
    def add(visited: g.NodeT) = {
      resultOrder(i) = visited.value
      i += 1
      Continue
    }

    (g get root).traverseNodes(ordering = nodeOrdering)(add)
    resultOrder should be (
        List(0 to 4: _*) ++
        List(11 to 13: _*) ++ List(21 to 23: _*) ++
        List(31 to 33: _*) ++ List(41 to 43: _*) toArray)

    i = 0
    (g get root).traverseNodes(breadthFirst = false,
                               ordering     = nodeOrdering)(add)
    resultOrder should be ((0 ::
        List(1) ::: List(11 to 13: _*) ::: List(2) ::: List(21 to 23: _*) :::
        List(3) ::: List(31 to 33: _*) ::: List(4) ::: List(41 to 43: _*)) toArray)
  }
  def test_EdgeOrdering {
    val root = 1
    val outerEdges = Seq[GraphParamIn[Int,WDiEdge]](
        root~>4 % 2, root~>2 % 5, root~>3 % 4,
           3~>6 % 4,    3~>5 % 5,    3~>7 % 2)
    val g = factory(outerEdges: _*)
    def edgeOrdering = g EdgeOrdering(g.Edge.WeightOrdering.reverse.compare)
    val resultOrder = new Array[Int](g.order)
    var i = 0
    def add(visited: g.NodeT) = {
      resultOrder(i) = visited.value
      i += 1
      Continue
    }

    (g get root).traverseNodes(ordering = edgeOrdering)(add)
    resultOrder should be (List(1 to 7: _*) toArray)
    
    i = 0
    (g get root).traverseNodes(breadthFirst = false,
                               ordering     = edgeOrdering)(add)
    resultOrder should be (List(1,2,3,5,6,7,4) toArray)
  }
}
