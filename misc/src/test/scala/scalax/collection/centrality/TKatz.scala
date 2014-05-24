package scalax.collection.centrality

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TKatzTest extends Spec with ShouldMatchers {

  def test_Wikipedia {
    val ( kim,  pria,  sri,  jose,  diego,  agneta,  aziz,  bob,  john,  jane,  samantha) =
        ("Kim","Pria","Sri","Jose","Diego","Agneta","Aziz","Bob","John","Jane","Samantha")

    val network = Graph.from(edges = List(
        kim ~ pria, kim ~ sri, pria ~ sri, pria ~ jose, sri ~ jose,
        jose ~ diego, diego ~ agneta, agneta ~ aziz, aziz ~ jose,
        jose ~ bob, bob ~ john, john ~ jane, jane ~ aziz, jane ~ samantha
    ))
    
    import Katz._
    val centralities = network.centralities[network.type]()
    centralities should be ('nonEmpty)

    // ordering for centrality maps with path dependent node types
    implicit def ord = centralityMapOrdering[String,UnDiEdge,network.type](centralities)
    centralities.max._1 should be (network get jose)
    
    // ordering for centrality maps with type projection node types
    val pCentralities = centralities: Map[_ <: Graph[String,UnDiEdge]#NodeT, Float]
    implicit def projectionOrd = centralityProjectionMapOrdering(pCentralities)
    pCentralities.min._1 should be (network get samantha)
  }
}