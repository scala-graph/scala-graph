package scalax.collection.constrained
package constraints

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, postfixOps}

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.constrained.generic.GraphConstrainedCompanion

@RunWith(classOf[JUnitRunner])
class TAcyclicRootTest
    extends Suites(
      new TAcyclic[immutable.Graph](immutable.Graph),
      new TAcyclic[mutable.Graph](mutable.Graph),
      new TAcyclicMutable)

class TAcyclicMutable extends RefSpec with Matchers {

  import mutable.Graph

  object `The 'Acyclic' constraint works fine with` {
    def `directed mutable graphs` {
      implicit val config: Config = Acyclic
      val g                       = Graph(1 ~> 2, 2 ~> 3)
      (g +=? 3 ~> 1) should be('left)
      g + 3 ~> 4 should have size (7)
    }
  }
}

class TAcyclic[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC] with GraphOps[N, E, CC]](
    val factory: GraphConstrainedCompanion[CC])
    extends RefSpec
    with Matchers
    with Testing[CC] {

  info("factory = " + factory.getClass)

  implicit val config: Config = Acyclic

  object `The 'Acyclic' constraint works fine with` {
    def `directed graphs` {
      val g = factory(1 ~> 2, 2 ~> 3)
      (g +? 3 ~> 1) should be('left)
      g + 3 ~> 4 should have size (7)
    }
    def `directed hypergraphs` {
      val g = factory[Int, HyperEdge](1 ~> 2 ~> 3, 2 ~> 3 ~> 4)
      (g +? 4 ~> 2) should be('left)
      g + 1 ~> 4 should have size (7)
    }
    def `undirected graphs` {
      val g = factory(1 ~ 2, 2 ~ 3)
      (g +? 3 ~ 1) should be('left)
      g + 3 ~ 4 should have size (7)
    }
    // TODO: GraphTraversal findCycle
//    def `hypergraphs ` {
//      val g = factory[Int,HyperEdge](1~2~3, 3~4~5)
//      a [CycleException] should be thrownBy { g + 4~2 }
//      a [CycleException] should be thrownBy { g + 1~4 }
//      g + 1~6 should have size (9)
//    }
    def `self loops #76` {
      //@todo how to test factory creation
//      a[CycleException] should be thrownBy { factory(1 ~> 1) }
      val g = factory[Int, DiEdge]()
      (g +? 1 ~> 1) should be('left)
    }
  }
}
