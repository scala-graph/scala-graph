package scalax.collection

import scala.language.{higherKinds, postfixOps}
import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.Random

import GraphPredef._, GraphEdge._
import GraphTraversal._, GraphTraversal.Parameters._ 
import generic.GraphCoreCompanion
import edge.WDiEdge, edge.WUnDiEdge, edge.Implicits._
import generator.GraphGen

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TConnectivityRootTest
	extends Suites( 
			new TConnectivity[immutable.Graph](immutable.Graph),
			new TConnectivity[  mutable.Graph](  mutable.Graph)
		)

final class TConnectivity[G[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G]]
			(val factory: GraphCoreCompanion[G])
	extends	RefSpec
	with	Matchers
	with	PropertyChecks {
  
  implicit val config = PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)
  
  object `In a weekly connected diGraph` {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)
    
    def `there exists no pair of mutually reachable nodes` {
      g.nodes.toList.combinations(2) foreach {
        case List(a, b) => List(a pathTo b, b pathTo a) should contain (None)
      }
    }
    
    def `evaluating strong components from any node yields single-node components` {
      g.nodes foreach { n =>
        val components = n.innerNodeTraverser.strongComponents
        components foreach (_.nodes should have size (1))
      }
    }
      
    def `evaluating all strong components yields a component for every node` {
      g.strongComponentTraverser().size should be (g.order)
    }
  }
  
  object `Having two strong comopnents` {
    // see example on https://de.wikipedia.org/wiki/Algorithmus_von_Tarjan_zur_Bestimmung_starker_Zusammenhangskomponenten
    val sccExpected = Vector[G[Symbol,DiEdge]](
        factory('a ~> 'b, 'b ~> 'c, 'c ~> 'd, 'd ~> 'a, 'd ~> 'e, 'c ~> 'e, 'e ~> 'c),
        factory('f ~> 'g, 'g ~> 'f, 'g ~> 'h, 'h ~> 'j, 'j ~> 'i, 'i ~> 'g, 'i ~> 'f, 'f ~> 'i)
    )
    assert(sccExpected.size == 2)
    assert(sccExpected(0).intersect(sccExpected(1)) == factory.empty)
    
    def `each is detected as such` { 
      sccExpected foreach (_.strongComponentTraverser() should have size (1))
    }
        
    def `connected by a diEdge yields a graph with the very same two strong comopnents` {
      val r = new Random
      val union = (factory.empty[Symbol,DiEdge] /: sccExpected)((r, g) => g union r)  
      val connectors = {
        def pickNode(index: Int) = sccExpected(index).nodes.draw(r).toOuter
        for (i <- 1 to 10) yield pickNode(0) ~> pickNode(1) 
      }
      connectors foreach { connector =>
        val connected = union + connector
        def check(scc: Traversable[connected.Component], expectedSize: Int): Unit = {
          scc should have size (expectedSize)
          scc foreach { sc =>
            sccExpected should contain (sc.toGraph)
            sc.frontierEdges should have size (1)
          }
        }
        
        check(connected.strongComponentTraverser().toVector, 2)
        
        val start = connected.nodes.draw(r) 
        check(
            start.innerNodeTraverser.strongComponents.toVector,
            if (sccExpected(0) contains start) 2 else 1
        )
      }
    }
  }
}
