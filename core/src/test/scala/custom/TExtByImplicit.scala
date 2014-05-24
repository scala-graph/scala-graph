package custom

import language.{higherKinds, implicitConversions, postfixOps, reflectiveCalls}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TExtByImplicitTest
	extends	Spec
	with	  ShouldMatchers
{
  def test_graphEnrichment {
    /* Provide graph enrichment.
     */
    implicit final class ExtGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) {
      
      /* Set of all directed edges contained in g.
       */
      def diEdges = g.edges filter (_.directed)
    }
    /* Consume graph enrichment.
     */
    Graph(1~2,  2~3).diEdges should be ('isEmpty)
    Graph(1~2, 2~>3).diEdges should have size (1)
  }
  
  def test_unDiGraphEnrichment {
    /* Provide graph enrichment restricted to undirected graphs.
     * Note that a lower bound must also be defined to exclude directed edges.
     */
    implicit final class UnDiGraph[N, E[X] >: UnDiEdge[X] <: UnDiEdge[X]]
        (val g: Graph[N,E]) {
      def alwaysTrue = true
    }
    /* Consume enrichment.  
     */
    Graph(1~2).alwaysTrue should be (true)
    /* Comment in the following lines to assure of getting compiler errors
     * when using other then undirected graphs.
     */
    // Graph(1~2~3).alwaysTrue 
    // Graph(1~>2).alwaysTrue
  }
  
  def test_nodeEnrichment_1 {
    /* Provide node enrichment.
     * Note that this way of enrichment is not suitable for methods returning
     * inner nodes or inner edges.
     */
    implicit final class ExtGraphNode[N, E[X] <: EdgeLikeIn[X]]
        (node_ : Graph[N,E]#NodeT) {
      type NodeT = graph.NodeT
      val graph = node_.containingGraph
      val node  = node_.asInstanceOf[NodeT]

      /* Calculates the sum of weights of outgoing edges from this node.
       */
      def outgoingWeights = node.outgoing.map(_.weight).sum
    }
    /* Consume node enrichment.
     */
    import scalax.collection.edge.Implicits._

    val g = Graph((1~%2)(1),  (1~%3)(2), (2~%3)(3))
    (g get 1).outgoingWeights should be (3)
    (g get 2).outgoingWeights should be (4)
    (g get 3).outgoingWeights should be (5)
  }
  
  def test_nodeEnrichment_2 {
    /* Provide node enrichment also suitable for methods returning inner nodes.
     * Note that compared with test_nodeEnrichment_3, `require` and `new Object`
     * lead to higher resource consumption. 
     */
    implicit def toExtNode[N, E[X] <: EdgeLikeIn[X]]
        (node: Graph[N,E]#NodeT)(implicit graph: Graph[N,E]) = {
      require(node.containingGraph eq graph,
          "'node' must be an inner node of the implicit 'graph' instance.")
      new Object {
        def inHeadOption = node.inNeighbors.headOption.
            asInstanceOf[Option[graph.NodeT]]
      }
    }
    /* Consume node enrichment.
     */
    implicit val g = Graph(1 ~> 2, 2 ~> 3, 2 ~> 1)
    (g get 1).inHeadOption.map(_.shortestPathTo(g get 3)()) should be ('isDefined)
  }

  def test_nodeEnrichment_3 {
    /* Provide node enrichment also suitable for methods returning inner nodes.
     * This is the recommended, type safe way of extending nodes. 
     */
    final class ExtGraph[N, E[X] <: EdgeLikeIn[X], G <: Graph[N,E]](val graph: G) {
      implicit final class ExtNode(node: graph.NodeT) {
        def inHeadOption = node.inNeighbors.headOption
      }
    }
    /* Consume node enrichment.
     * You need to instantiate `ExtGraph` for each graph separately and import
     * the contained implicit conversion.
     */
    val g = Graph(1 ~> 2, 2 ~> 3, 2 ~> 1)
    val enriched = new ExtGraph[Int,DiEdge,g.type](g)
    import enriched.ExtNode

    (g get 1).inHeadOption.map(_.shortestPathTo(g get 3)) should be ('isDefined)
  }
}