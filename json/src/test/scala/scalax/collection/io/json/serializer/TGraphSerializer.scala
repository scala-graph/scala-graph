package scalax.collection.io.json
package serializer

import language.higherKinds

import net.liftweb.json._

import org.scalatest.{Spec, Suites}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.GraphPredef._
import scalax.collection._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scalax.collection.io.json.descriptor.StringNodeDescriptor

@RunWith(classOf[JUnitRunner])
class TGraphSerializerRootTest
  extends Suites(
      new TGraphSerializer[immutable.Graph](immutable.Graph),
      new TGraphSerializer[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers

/** Tests the serialization of any class, here `class Container`, containing a graph.
 */
class TGraphSerializer[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
    extends Spec
    with ShouldMatchers {

  object GraphFixture {
    val graphJsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          ["A", "B", 1],
          ["B", "A", 2],
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
                         StringNodeDescriptor,
                         new WEdgeDescriptor[String,WDiEdge,WDiEdge.type](
                             WDiEdge, Some(new WEdgeSerializer))
                     )
    val graph = factory(("A"~%>"B")(1), ("B"~%>"A")(2))
  }
  import GraphFixture._
  
  object ContainerFixture {

    val container = Container(0, graph)
    val containerJsonText =
      """{ "i" : 0, "g" : %s}""" filterNot (_.isWhitespace) format graphJsonText
  }

  def test_importWEdge {
    val g = factory.fromJson[String,WDiEdge](graphJsonText, descriptor)
    g should equal (graph)
  }
  
  def test_ImExWEdge {
    val g = factory.fromJson[String,WDiEdge](graphJsonText, descriptor)
    factory.fromJson[String,WDiEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  def test_Container {
    import ContainerFixture._
    
    implicit val format: Formats = DefaultFormats +
      new GraphSerializer(descriptor)
    
    val decomposed = Extraction.decompose(container)
    val decomopsedText = compact(render(decomposed))
    
    val ast = JsonParser.parse(decomopsedText)
    val extracted = ast.extract[Container]
    
    extracted should equal (container)
  }
}
protected case class Container(i: Int, g: Graph[String, WDiEdge])