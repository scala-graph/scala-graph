package scalax.collection
package io.json

import scalax.collection._
import scalax.collection.OuterImplicits._
import scalax.collection.generic.{Edge, GenericGraphCoreFactory}
import descriptor.predefined._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.Suites

import scalax.collection.edges.DiEdge

class DefaultSerializationSpec
    extends Suites(
      new DefaultSerialization[immutable.Graph](immutable.Graph),
      new DefaultSerialization[mutable.Graph](mutable.Graph)
    )

private class DefaultSerialization[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  private object Fixture {
    val jsonText = """
      { "nodes" : [
          {"i":1, "e":{"jsonClass":"CExt","i":2}}
        ]
      }""".filterNot(_.isWhitespace)

    def descriptor(extClasses: List[Class[_]]) =
      Descriptor.simple[Node](
        new NodeDescriptor[Node](extraClasses = extClasses) {
          def id[B >: Node](node: B): String = node.toString
        },
        Di.descriptor[Node]()
      )

    val extClasses = List(classOf[CExt])
    val graph      = factory[Node, DiEdge](Node(1, CExt(2)))
  }
  import Fixture._

  object `JSON import/export of node classes not known at compile time works fine` {
    def `when exporting`(): Unit =
      graph.toJson(descriptor(extClasses)) shouldBe jsonText

    def `when importing`(): Unit =
      factory.fromJson[Node, DiEdge[Node]](jsonText, descriptor(extClasses)) shouldBe graph

    def `when reimporting`(): Unit =
      factory.fromJson[Node, DiEdge[Node]](graph.toJson(descriptor(extClasses)), descriptor(extClasses)) shouldBe graph
  }
}

trait Ext
case class Node(i: Int, e: Ext)
// library user extension
case class CExt(i: Int) extends Ext
