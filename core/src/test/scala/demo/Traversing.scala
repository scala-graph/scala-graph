package demo

import scala.language.higherKinds

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.Graph

import org.scalatest.Spec
import org.scalatest.Matchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-traversing.html
 *  Traversing Graphs]]. 
 */
@RunWith(classOf[JUnitRunner])
final class TraversingTest
    extends Spec
       with Matchers {

  import scalax.collection.edge.{WDiEdge, WUnDiEdge}
  import scalax.collection.edge.Implicits._

  def validatePath[N, E[X] <: EdgeLikeIn[X]](p: Graph[N,E]#Path,
                                             sample: Traversable[Param[N,E]]): Unit = {
    def toN(p: Param[N,E]): N = p match {
      case OuterNode(n) => n
      case _ => throw new IllegalArgumentException
    }
    p.toList == sample ||
    p.isValid && p.startNode == toN(sample.head) &&
                 p.endNode   == toN(sample.last) should be (true)
  }

  val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
                3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
  def n(outer: Int): g.NodeT = g get outer
  
  def `for a result` {
    
    n(1) findSuccessor (_.outDegree >  3)              should be (None) 
    n(1) findSuccessor (_.outDegree >= 3)              should be (Some(3)) 
    n(4) findSuccessor (_.edges forall (_.undirected)) should be (Some(2))
    n(4) isPredecessorOf n(1)                          should be (true)
                                 validatePath[Int,WUnDiEdge]((
    n(1) pathTo n(4)
                                 ).get, List(1, 1~>3 %5, 3, 3~4 %1, 4))
                                 validatePath[Int,WUnDiEdge]((
    n(1) pathUntil (_.outDegree >= 3)
                                 ).get, List(1, 1~>3 %5, 3))
    val spO = n(3) shortestPathTo n(1)
    val sp = spO.get
                                 validatePath[Int,WUnDiEdge](sp,
                                 List(3, 3~4 %1, 4, 4~>5 %0, 5, 1~5 %3, 1))
    sp.nodes                     .toList should be (List(3, 4, 5, 1))
    sp.weight                    should be (4)

    def negWeight(e: g.EdgeT): Float = 5.5f - e.weight
    val spNO = n(3) shortestPathTo (n(1), negWeight)
    val spN = spNO.get
                                 validatePath[Int,WUnDiEdge](sp,
                                 List(3, 2~3 %2, 2, 1~>2 %4, 1))
    spN.nodes                    .toList should be (List(3, 2, 1))
    spN.weight                   should be (6)
    
    val pO1 = n(4).withSubgraph(nodes = _ < 4) pathTo n(2)
                                 validatePath[Int,WUnDiEdge](pO1.get,
                                 List(4, 3~4 %1, 3, 2~3 %2, 2))
    pO1.map(_.nodes)             .get.toList should be (List(4, 3, 2)) 
    
    val pO2 = n(4).withSubgraph(edges = _.weight != 2) pathTo n(2)
                                 validatePath[Int,WUnDiEdge](pO2.get,
                                 List(4, 4~>5 %0, 5, 1~5 %3, 1, 1~2 %4, 2))
    pO2.map(_.nodes)             .get.toList should be (List(4, 5, 1, 2)) 
  }

  def `cycle detecting` {
    val g = Graph(1~>2, 1~>3, 2~>3, 3~>4, 4~>2)
    val fc1 = g.findCycle
                                 fc1.get.sameElements(List(
                                 2, 2~>3, 3, 3~>4, 4, 4~>2, 2)) should be (true)
    val fc2 = (g get 4).findCycle
                                 fc2.get.sameElements(List(
                                 4, 4~>2, 2, 2~>3, 3, 3~>4, 4)) should be (true)
    for (c1 <- fc1; c2 <- fc2) yield c1 == c2          should be (false)
    for (c1 <- fc1; c2 <- fc2) yield c1 sameAs c2      should be (true)
  }
  
  def `ordered traversal` {
    val root = 1
    val g = Graph(root~>4 % 2, root~>2 % 5, root~>3 % 4,
                     3~>6 % 4,    3~>5 % 5,    3~>7 % 2)
    
    def edgeOrdering = g.EdgeOrdering(g.Edge.WeightOrdering.reverse.compare)
    val traverser = (g get root).outerNodeTraverser.withOrdering(edgeOrdering)
     
    traverser.toList             should be (List(1,2,3,4,5,6,7))
  }
  
  def `traversers with fluent properties` {
    val g = Graph(1~>2 % 1, 1~>3 % 2, 2~>3 % 3, 3~>4 % 1)
    val n1 = g get 1
    
    n1.outerNodeTraverser.sum                 should be (10)
    g.outerNodeTraverser(n1).sum              should be (10)
    n1.outerNodeTraverser.withMaxDepth(1).sum should be (6)
    
    n1.innerEdgeTraverser.map(_.weight).sum   should be (7)
    
    n1.innerElemTraverser.filter(_ match {
      case g.InnerNode(n) => n.degree > 1
      case g.InnerEdge(e) => e.weight > 1
    })                           .map[OuterElem[Int,WDiEdge],
                                      Traversable[OuterElem[Int,WDiEdge]]]((_: g.InnerElem) match {
                                   case g.InnerNode(n) => n.value
                                   case g.InnerEdge(e) => e.toOuter
                                 }).toSet should be (Set[OuterElem[Int,WDiEdge]](
                                 1, 2, 3, 1~>3 % 2, 2~>3 % 3))
  }
  
  def `DownUp traverser` {
    import scala.collection.mutable.ArrayBuffer

    val root = "A"
    val g = Graph(root~>"B1", root~>"B2")
    val innerRoot = g get root
    val result = (ArrayBuffer.empty[String] /: innerRoot.innerNodeDownUpTraverser) {
        (buf, param) => param match {
          case (down, node) => 
            if (down) buf += (if (node eq innerRoot) "(" else "[") += node.toString
            else      buf += (if (node eq innerRoot) ")" else "]")
        }
    }
    ("" /: result)(_+_)          should (be ("(A[B1][B2])")
                                      or be ("(A[B2][B1])"))
  }

  def `extended traverser` {
    val g = Graph(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4, 4 ~> 2)

    import g.ExtendedNodeVisitor
    import scalax.collection.GraphTraversal._
    import scalax.collection.GraphTraversalImpl._

    type ValDepth = (Int,Int)
    var info = List.empty[ValDepth]
    (g get 1).innerNodeTraverser.withKind(DepthFirst).foreach {
      ExtendedNodeVisitor((node, count, depth, informer) => {
        info :+= (node.value, depth)
      })
    }
    info.sortWith((a: ValDepth, b: ValDepth) =>
      a._1  < b._1 ||
      a._1 == b._1 && a._2 < b._2) should be (List((1,0), (2,1), (3,1), (4,2)))    
  }
  
  def test_Combined {
    val g = Graph(1~>2, 1~>3, 2~>3, 3~>4, 4~>2)
    var center: Option[g.NodeT] = None
                                 val c =
   (g get 4).findCycle( n =>
     center = center match {
       case s @ Some(c) => if (n.degree > c.degree) Some(n) else s
       case None        => Some(n)
     }
   )
                                 c.get.sameElements(List(
                                 2, 2~>3, 3, 3~>4, 4, 4~>2, 2)) should be (true)
                                 center.get should be (2)
  }

  def `component traverser` {
    def someEdges(i: Int) =
      List((i) ~> (i + 1), (i) ~> (i + 2), (i + 1) ~> (i + 2))

    val disconnected = Graph.from(edges = someEdges(1) ++ someEdges(5))
    val sums =
      for (c <- disconnected.componentTraverser())
        yield c.nodes.head.outerNodeTraverser.sum
    sums should be (List(6, 18))
  }
  
  def `component traverser #57` {
    val g = Graph(11~>12, 13~>14)
    g.componentTraverser() should have size (2)
  }
  
  def `path builder` {
    val builder = g.newPathBuilder(n(1))
    builder += n(3) += n(4)
    builder.result               .toString should be ("Path(1, 1~>3 %5, 3, 3~4 %1, 4)")
   
    builder.clear
    builder += n(4) += n(3)
    builder.result               .toString should be ("Path(1, 1~>3 %5, 3)")
    
    builder.clear
    builder add n(4)             should be (false)
  }
}