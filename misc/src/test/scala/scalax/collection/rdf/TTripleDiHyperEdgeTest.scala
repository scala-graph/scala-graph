package scalax.collection
package rdf

import org.scalatest.refspec.RefSpec
import org.scalatest.Matchers

class TTripleDiHyperEdgeTest extends RefSpec with Matchers {

  type Triple[N] = TripleDiHyperEdge[N]
  val Triple = TripleDiHyperEdge

  val (subj_1, subj_2) = (IRI("s-1"), IRI("s-2"))
  val pred             = IRI("p-1")
  val (obj_1, obj_2)   = (Label("l-1"), BlankNode(1))
  val initialTriples = List[Triple[RdfNode]](
      Triple(subj_1, pred, obj_1),
      Triple(subj_2, pred, obj_2)
  )
  
  object `The custom directed edge 'TripleDiHyperEdge'` {
    def `has the expected properties` {
      val stmt = initialTriples.head
      
      stmt.subject   should be (subj_1)
      stmt.predicate should be (pred)
      stmt.`object`  should be (obj_1)
    }
  }

  object `A triple store based on the custom directed edge 'TripleDiEdge'` {
    def `may be created` {
      val g: Graph[RdfNode,Triple] = Graph.from(edges = initialTriples)
  
      g.order should be (5)
      g.graphSize should be (initialTriples.size)
    }
    def `has inner edges with the expected properties` {
      val g = Graph.from(edges = initialTriples)
      val stmt = g.get(initialTriples.head)
      
      stmt.subject   should be (subj_1)
      stmt.predicate should be (pred)
      stmt.`object`  should be (obj_1)
    }
  }
}