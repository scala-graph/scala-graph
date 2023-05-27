package scalax.collection
package io.json
package serializer

import net.liftweb.json._

import scalax.collection.OuterImplicits._
import scalax.collection.generic._
import scalax.collection.edges._
import scalax.collection.edges.labeled._
import scalax.collection.io.json.descriptor.StringNodeDescriptor
import scalax.collection.io.json.descriptor.predefined.WDi
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.Suites

class GraphSerializerSpec
    extends Suites(
      new Serialize[immutable.Graph](immutable.Graph),
      new Serialize[mutable.Graph](mutable.Graph)
    )

private class Serialize[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  private object ContainedGraphFixture {
    val graphJsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          ["A", "B", 1.0],
          ["B", "A", 2.0],
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
      StringNodeDescriptor,
      WDi.descriptor[String]()
    )
    val graph = factory("A" ~> "B" % 1.0, "B" ~> "A" % 2.0)
  }
  import ContainedGraphFixture._

  private object ContainerFixture {
    val container = Container(0, graph)
    val containerJsonText =
      """{ "i" : 0, "g" : %s}""" filterNot (_.isWhitespace) format graphJsonText
  }

  object `JSON serialization of any class containing a graph` {
    def `when exporting`(): Unit = {
      val g = factory.fromJson[String, WDiEdge[String]](graphJsonText, descriptor)
      g should equal(graph)
    }

    def `when reimporting`(): Unit = {
      val g = factory.fromJson[String, WDiEdge[String]](graphJsonText, descriptor)
      factory.fromJson[String, WDiEdge[String]](g.toJson(descriptor), descriptor) should equal(g)
    }

    def `when decomposing and parsing`(): Unit = {
      import ContainerFixture._
      implicit val format: Formats = DefaultFormats + new GraphSerializer(descriptor)

      val decomposed     = Extraction.decompose(container)
      val decomposedText = compactRender(decomposed)

      val ast       = JsonParser.parse(decomposedText)
      val extracted = ast.extract[Container]

      extracted should equal(container)
    }
  }
}

protected case class Container(i: Int, g: AnyGraph[String, WDiEdge[String]])
