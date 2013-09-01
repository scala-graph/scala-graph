package scalax.collection.centrality

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TKatz extends Suite with ShouldMatchers {

  def test_Wikipedia {
    val ( kim,  pria,  sri,  jose,  diego,  agneta,  aziz,  bob,  john,  jane,  samantha) =
        ("Kim","Pria","Sri","Jose","Diego","Agneta","Aziz","Bob","John","Jane","Samantha")
    val connections = Set(
        kim ~ pria, kim ~ sri, pria ~ sri, pria ~ jose, sri ~ jose,
        jose ~ diego, diego ~ agneta, agneta ~ aziz, aziz ~ jose,
        jose ~ bob, bob ~ john, john ~ jane, jane ~ aziz, jane ~ samantha
    )
    val network = Graph.from(edges = connections)
    
    import Katz._
    val centralities = network.centralities()
 
    implicit def ordering = centralities.ordering
    // we need a projection for the provided ordering
    val pCentralities = centralities: Map[_ <: Graph[String,UnDiEdge]#NodeT, Float]
    
    pCentralities.max._1 should be (network get jose)
    pCentralities.min._1 should be (network get samantha)
  }
}