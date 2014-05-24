package custom

import language.postfixOps

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import immutable.{MyExtGraph => Graph}
import mutable.{MyExtGraph => MutableGraph}

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TExtNodeTest
	extends	Spec
	with	  ShouldMatchers
{
  def test_helloAdjacents {
    val g = Graph(1~2, 1~3)
    (g get 1 helloSuccessors) should be ("Hello 2,3!")
  }
  def test_mutableHelloAdjacents {
    val g = MutableGraph(1~2, 1~3)
    val n1 = g get 1 
    (n1 helloSuccessors) should be ("Hello 2,3!")
    g += 1~4
    (n1 helloSuccessors) should be ("Hello 2,3,4!")
  }
}