package scalax.collection.centrality

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

class TKatzTest extends RefSpec with Matchers {

  object `Katz centralities naive implementation works fine with an example at Wikipedia` {
    val ( kim,  pria,  sri,  jose,  diego,  agneta,  aziz,  bob,  john,  jane,  samantha) =
        ("Kim","Pria","Sri","Jose","Diego","Agneta","Aziz","Bob","John","Jane","Samantha")
  
    val network = Graph.from(edges = List(
        kim ~ pria, kim ~ sri, pria ~ sri, pria ~ jose, sri ~ jose,
        jose ~ diego, diego ~ agneta, agneta ~ aziz, aziz ~ jose,
        jose ~ bob, bob ~ john, john ~ jane, jane ~ aziz, jane ~ samantha))
      
    import Katz._
    val centralities: Map[network.NodeT,Float] = network.centralities()
    
    def `yielding a non-empty map` {
      centralities should be ('nonEmpty)
    }

    def `using path dependent node types` {
      implicit def ord = centralityMapOrdering[String,UnDiEdge,network.type](centralities)
      centralities.max._1 should be (network get jose)
    }
    
    def `using type projection node types` {
      val pCentralities = centralities: Map[_ <: Graph[String,UnDiEdge]#NodeT, Float]
      implicit def projectionOrd = centralityProjectionMapOrdering(pCentralities)
      pCentralities.min._1 should be (network get samantha)
    }
  }
}