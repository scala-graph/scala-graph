package custom

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

@RunWith(classOf[JUnitRunner])
class TExtByImplicitTest
	extends	Suite
	with	  ShouldMatchers
{
  def test_graphEnrichment {
    /*
     * Enriches Graph with custom methods.
     */
    final class ExtGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) {
      /*
       * Set of all directed edges contained - makes sense for mixed graphs only.
       */
      def diEdges = g.edges filter (_.directed)
    }
    implicit def gToExtG[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) =
      new ExtGraph[N,E](g)

    // test enrichment --------------
    Graph(1~2,  2~3).diEdges should be ('isEmpty)
    Graph(1~2, 2~>3).diEdges should have size (1)
  }
  
  def test_enrichedUnDiGraph {
    /*
     * Restrict enrichment to undirected graphs only.
     */
    final class UnDiGraph[N, E[X] >: UnDiEdge[X] <: UnDiEdge[X]](val g: Graph[N,E]) {
      def alwaysTrue = true
    }
    implicit def gToUnDiG[N, E[X] >: UnDiEdge[X] <: UnDiEdge[X]](g: Graph[N,E]) =
      new UnDiGraph[N,E](g)
    
    Graph(1~2).alwaysTrue should be (true)
    // must not compile:
    // Graph(1~2~3).alwaysTrue 
    // Graph(1~>2).alwaysTrue
  }
  
  def test_nodeEnrichment_1 {
    // provide enrichment --------------
    final class ExtGraphNode[N, E[X] <: EdgeLikeIn[X]](node_ : Graph[N,E]#NodeT)
    {
      type NodeT = graph.NodeT
      val graph = node_.containingGraph
      val node  = node_.asInstanceOf[NodeT]
      /*
       * Calculates the sum of weights of outgoing edges from this node.
       */
      def outgoingWeights = node.outgoing.map(_.weight).sum
    }
    implicit def nodeToExtN[N, E[X] <: EdgeLikeIn[X]] (node: Graph[N,E]#NodeT) =
      new ExtGraphNode[N,E](node)

    // consume enrichment --------------
    import scalax.collection.edge.Implicits._

    implicit val g = Graph((1~%2)(1),  (1~%3)(2), (2~%3)(3))
    (g get 1).outgoingWeights should be (3)
    (g get 2).outgoingWeights should be (4)
    (g get 3).outgoingWeights should be (5)
    
  }
  
  def test_nodeEnrichment_2 {
    // provide enrichment --------------
    implicit def toExtNode[N, E[X] <: EdgeLikeIn[X]]
        (node: Graph[N,E]#NodeT)(implicit graph: Graph[N,E]) = {
      require(node.containingGraph eq graph,
          "'node' must be an inner node of the implicit 'graph' instance.")
      new Object {
        def inHead = node.inNeighbors.head.asInstanceOf[graph.NodeT]
      }
    }
    // consume enrichment --------------
    implicit val g = Graph(1 ~> 2, 2 ~> 3)
    (g get 1).inHead.shortestPathTo(g get 3) should be ('isDefined)
  }

  def test_nodeEnrichment_3 {
    // provide enrichment --------------
    class ExtGraph[N, E[X] <: EdgeLikeIn[X], G <: Graph[N,E]](val graph: G) {
      implicit final class ExtNode(node: graph.type#NodeT) {
        def inHead = node.inNeighbors.head
      }
    }
    // consume enrichment --------------
    val g = Graph(1 ~> 2, 2 ~> 3)
    val e = new ExtGraph[Int,DiEdge,g.type](g)
    import e.ExtNode
    (g get 1).inHead.shortestPathTo(g get 3) should be ('isDefined)
  }
}