package scalax.collection.io.json
package serializer

import language.higherKinds

import net.liftweb.json._

import scalax.collection.GraphPredef._
import scalax.collection._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scalax.collection.io.json.descriptor.StringNodeDescriptor

import org.scalatest._
import org.scalatest.refspec.RefSpec
class TGraphSerializerRootTest
    extends Suites(
      new TGraphSerializer[immutable.Graph](immutable.Graph),
      new TGraphSerializer[mutable.Graph](mutable.Graph))

class TGraphSerializer[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers {

  private object GraphFixture {
    val graphJsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          ["A", "B", 1.0],
          ["B", "A", 2.0],
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
      StringNodeDescriptor,
      new WEdgeDescriptor[String, WDiEdge, WDiEdge.type](WDiEdge, Some(new WEdgeSerializer))
    )
    val graph = factory(("A" ~%> "B")(1), ("B" ~%> "A")(2))
  }
  import GraphFixture._

  private object ContainerFixture {
    val container = Container(0, graph)
    val containerJsonText =
      """{ "i" : 0, "g" : %s}""" filterNot (_.isWhitespace) format graphJsonText
  }

  object `JSON serialization of any class containing a graph works fine` {
    def `when exporting` {
      val g = factory.fromJson[String, WDiEdge](graphJsonText, descriptor)
      g should equal(graph)
    }
    def `when reimporting` {
      val g = factory.fromJson[String, WDiEdge](graphJsonText, descriptor)
      factory.fromJson[String, WDiEdge](g.toJson(descriptor), descriptor) should equal(g)
    }
    def `when decomposing and parsing` {
      import ContainerFixture._

      implicit val format: Formats = DefaultFormats +
        new GraphSerializer(descriptor)

      val decomposed     = Extraction.decompose(container)
      val decomposedText = compactRender(decomposed)

      val ast       = JsonParser.parse(decomposedText)
      val extracted = ast.extract[Container]

      extracted should equal(container)
    }
  }
}

protected case class Container(i: Int, g: Graph[String, WDiEdge])
