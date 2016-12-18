package scalax.collection.centrality

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TKatzTest extends RefSpec with Matchers {

  object `Katz centralities works fine with an example at Wikipedia such that` {
    val ( kim,  pria,  sri,  jose,  diego,  agneta,  aziz,  bob,  john,  jane,  samantha) =
        ("Kim","Pria","Sri","Jose","Diego","Agneta","Aziz","Bob","John","Jane","Samantha")
  
    val network = Graph.from(edges = List(
        kim ~ pria, kim ~ sri, pria ~ sri, pria ~ jose, sri ~ jose,
        jose ~ diego, diego ~ agneta, agneta ~ aziz, aziz ~ jose,
        jose ~ bob, bob ~ john, john ~ jane, jane ~ aziz, jane ~ samantha))
      
    import Katz._
    val centralities: Map[network.NodeT,Float] = network.centralities()
    println(centralities)
    
    def `it is not empty` {
      centralities should be ('nonEmpty)
    }

    def `ordering for centrality maps with path dependent node types` {
      implicit def ord = centralityMapOrdering[String,UnDiEdge,network.type](centralities)
      centralities.max._1 should be (network get jose)
    }
    
    def `ordering for centrality maps with type projection node types` {
      val pCentralities = centralities: Map[_ <: Graph[String,UnDiEdge]#NodeT, Float]
      implicit def projectionOrd = centralityProjectionMapOrdering(pCentralities)
      pCentralities.min._1 should be (network get samantha)
    }
  }
}