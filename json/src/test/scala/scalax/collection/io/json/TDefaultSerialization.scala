package scalax.collection.io.json

import language.higherKinds

import scalax.collection._
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCoreCompanion

import descriptor.predefined._

import org.scalatest._
import org.scalatest.refspec.RefSpec
class TDefaultSerializationRootTest
    extends Suites(
      new TDefaultSerialization[immutable.Graph](immutable.Graph),
      new TDefaultSerialization[mutable.Graph](mutable.Graph))

class TDefaultSerialization[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers {

  object Fixture {
    val jsonText = """
      { "nodes" : [
          {"i":1, "e":{"jsonClass":"CExt","i":2}}
        ]
      }""".filterNot(_.isWhitespace)
    def descriptor(extClasses: List[Class[_]]) =
      new Descriptor[Node](
        new NodeDescriptor[Node](extraClasses = extClasses) {
          def id(node: Any) = node.toString
        },
        Di.descriptor[Node]()
      )
    val extClasses = List(classOf[CExt])
    val graph      = factory[Node, DiEdge](Node(1, CExt(2)))
  }

  object `JSON import/export of node classes not known at compilation time works fine` {
    def `when exporting` {
      import Fixture._
      graph.toJson(descriptor(extClasses)) should be(jsonText)
    }
    def `when importing` {
      import Fixture._
      factory.fromJson[Node, DiEdge](jsonText, descriptor(extClasses)) should be(graph)
    }
    def `when reimporting` {
      import Fixture._
      factory.fromJson[Node, DiEdge](graph.toJson(descriptor(extClasses)), descriptor(extClasses)) should be(graph)
    }
  }
}

trait Ext
case class Node(val i: Int, val e: Ext)
// library user extension
case class CExt(i: Int) extends Ext
