package scalax.collection.io.json

import language.higherKinds

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import net.liftweb.json._

import scalax.collection._
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.edge._,
       scalax.collection.edge.Implicits._

import serializer._, imp._, imp.Parser.{parse => graphParse},
       descriptor._, descriptor.predefined._, descriptor.Defaults._,
       exp.Export
       
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TJsonRootTest
  extends Suites(
      new TJson[immutable.Graph](immutable.Graph),
      new TJson[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  // ---------------------------------------- mutable tests
}
/**	Tests JSON input and output.
 */
class TJson[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
	extends	Spec
	with	ShouldMatchers
{
  object FixtureMixed {
    /* JSON text to import:
     * - edge list with the default edge type
     * - two edge lists with given edge types
     * - some noise to be ignored
     */
    val jsonText = """
      { "nodes" : [["A"], ["B"], ["C"], ["X"], ["Y"]],
        "edges": [
          ["A", "B"],
          ["B", "C"]
        ],
        "edges": { "WkDiEdge": [
          ["A", "B", 3],
          ["B", "C", 4]]
        },
        "X": 1,
        "edges": { "UnDiEdge": [
          {"n1":"X", "n2":"Y"},
          {"n1":"Y", "n2":"A"}]
        }
      }""".filterNot(_.isWhitespace)
    val descriptor =
      new Descriptor[String](
          StringNodeDescriptor,
          UnDi.descriptor[String](Some(new EdgeSerializer)),
          namedEdgeDescriptors = Seq[GenEdgeDescriptor[String]](
            UnDi.descriptor[String](),
            WkDi.descriptor[String](Some(new WEdgeSerializer)))
      )
    val graph = factory("A"~"B", "B"~"C",
                        ("A"~%#>"B")(3), ("B"~%#>"C")(4),
                        "X"~"Y", "Y"~"A") 
  }

  def test_parseMixed {
    import FixtureMixed._
    val lists = graphParse(jsonText, descriptor).toList

    lists should have size (4)
    lists(0) match {
      case NodeList(nodeTypeId, _nodes) =>
        nodeTypeId should be (defaultId)
        _nodes should have size (5)
        val nodes = _nodes.toList
        nodes(0) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(A))")
          case _ => fail
        }
        nodes(4) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(A))")
          case _ => fail
        }
      case _ => fail
    }
    lists(1) match {
      case EdgeList(edgeTypeId, _edges) =>
        edgeTypeId should be (defaultId)
        _edges should have size (2)
        val edges = _edges.toList
        edges(0) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(A), JString(B))")
          case _ => fail
        }
        edges(1) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(B), JString(C))")
          case _ => fail
        }
      case _ => fail
    }
    lists(2) match {
      case EdgeList(edgeTypeId, _edges) =>
        edgeTypeId should be ("WkDiEdge")
        _edges should have size (2)
        val edges = _edges.toList
        edges(0) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(A), JString(B), JInt(3))")
          case _ => fail
        }
        edges(1) match {
          case JArray(fields) => fields.toString should be
                                ("List(JString(B), JString(C), JInt(4))")
          case _ => fail
        }
      case _ => fail
    }
    lists(3) match {
      case EdgeList(edgeTypeId, _edges) =>
        edgeTypeId should be ("UnDiEdge")
        _edges should have size (2)
        val edges = _edges.toList
        edges(0) match {
          case JObject(fields) => fields.toString should be
                                 ("List(JField(v1,JString(X)), JField(v2,JString(Y)))")
          case _ => fail
        }
        edges(1) match {
          case JObject(fields) => fields.toString should be
                                 ("List(JField(v1,JString(Y)), JField(v2,JString(A)))")
          case _ => fail
        }
        Unit
      case _ => fail
    }
  }
  def test_importMixed {
    import FixtureMixed._
    val g = factory.fromJson[String,UnDiEdge](jsonText, descriptor)
    g should equal (graph)
  }
  def test_ImExMixed {
    import FixtureMixed._
    val g = factory.fromJson[String,UnDiEdge](jsonText, descriptor)
    factory.fromJson[String,UnDiEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  object FixtureLEdge {
    val jsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          {"n1":"A", "n2":"B", "label":"L1"},
          {"n1":"B", "n2":"A", "label":"L2"}
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
                         StringNodeDescriptor,
                         LDi.descriptor[String,String](aLabel = ""))
    val graph = factory[String,LDiEdge](("A"~+>"B")("L1"),("B"~+>"A")("L2"))
  }
  def test_importLEdge {
    import FixtureLEdge._
    val g = factory.fromJson[String,LDiEdge](jsonText, descriptor)
    g should equal (graph)
  }
  def test_ImExLEdge {
    import FixtureLEdge._
    val g = factory.fromJson[String,LDiEdge](jsonText, descriptor)
    factory.fromJson[String,LDiEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  object FixtureLEdgeCustom {
    val jsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          ["A", "B", "L1"],
          ["B", "A", "L2"]
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
                         StringNodeDescriptor,
                         LDi.descriptor[String,String]("",
                             Some(new LEdgeSerializer[String](new StringSerializer))))
    val graph = FixtureLEdge.graph
  }
  def test_importLEdgeCustom {
    import FixtureLEdgeCustom._
    val g = factory.fromJson[String,LDiEdge](jsonText, descriptor)
    g should equal (graph)
  }
  def test_ImExLEdgeCustom {
    import FixtureLEdgeCustom._
    val g = factory.fromJson[String,LDiEdge](jsonText, descriptor)
    factory.fromJson[String,LDiEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  object FixtureWLEdgeCustom {
    val jsonText = """
      { "nodes" : [["A"], ["B"]],
        "edges": [
          ["A", "B", 100, "CLabel-1"],
          ["B", "A", 200, "CLabel-2"],
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
                         StringNodeDescriptor,
                         new WLEdgeDescriptor[String,WLDiEdge,WLDiEdge.type,String](
                             WLDiEdge, "", Some(new WLEdgeSerializer[String]
                                                   (new StringSerializer)))
                     )
    val graph = factory(("A"~%+>"B")(100, "CLabel-1"),("B"~%+>"A")(200, "CLabel-2"))
  }
  def test_importWLEdgeCustom {
    import FixtureWLEdgeCustom._
    val g = factory.fromJson[String,WLDiEdge](jsonText, descriptor)
    g should equal (graph)
  }
  def test_ImExWLEdgeCustom {
    import FixtureWLEdgeCustom._
    val g = factory.fromJson[String,WLDiEdge](jsonText, descriptor)
    factory.fromJson[String,WLDiEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  object FixtureDiHyperEdgeCustom {
    val jsonText = """
      { "nodes" : [["A"], ["B"], ["C"]],
        "edges": [
          ["A", "B"],
          ["B", "A", "C"],
        ]
      }""".filterNot(_.isWhitespace)
    val descriptor = new Descriptor[String](
                         StringNodeDescriptor,
                         new HyperEdgeDescriptor[String,DiHyperEdge,DiHyperEdge.type](
                             DiHyperEdge, Some(new HyperEdgeSerializer))
                     )
    val graph = factory[String,DiHyperEdge](DiHyperEdge("A","B"), "B"~>"A"~>"C")
  }
  def test_importDiHyperEdgeCustom {
    import FixtureDiHyperEdgeCustom._
    val jsonText = """
      { "nodes" : [["A"], ["B"], ["C"]],
        "edges": [
          ["A", "B"],
          ["B", "A", "C"],
        ]
      }""".filterNot(_.isWhitespace)
    val g = factory.
      fromJson[String,DiHyperEdge](jsonText,
               new Descriptor[String](
                   StringNodeDescriptor,
                   new HyperEdgeDescriptor[String,DiHyperEdge,DiHyperEdge.type](
                       DiHyperEdge, Some(new HyperEdgeSerializer))
               )
    )
    g should equal (graph)
  }
  def test_ImExDiHyperEdgeCustom {
    import FixtureDiHyperEdgeCustom._
    val g = factory.fromJson[String,DiHyperEdge](jsonText, descriptor)
    factory.fromJson[String,DiHyperEdge](g.toJson(descriptor), descriptor) should equal (g)
  }

  def test_importWLHyperEdge {
    val jsonText = """
      { "nodes" : [["A"], ["B"], ["C"]],
        "edges": [
          [["A", "B"]     , 100, "Label-1"],
          [["B", "A", "C"], 200, "Label-2"],
        ]
      }""".filterNot(_.isWhitespace)
    val g: CC[String,WLHyperEdge] = factory.
      fromJson[String,WLHyperEdge](jsonText,
               new Descriptor[String](
                   StringNodeDescriptor,
                   new WLHyperEdgeDescriptor[String,WLHyperEdge,WLHyperEdge.type,String](
                       WLHyperEdge, "", Some(new WLHyperEdgeSerializer[String]
                                                (new StringSerializer)))
               )
    )
    g.nodes.toSortedString() should be ("NodeSet(A, B, C)")
    g.edges.toSortedString() should be ("EdgeSet(A~B %100 'Label-1, B~A~C %200 'Label-2)")
  }
  def test_exp {
    val g = factory("B"~>"A"~>"C")
    val descr = new Descriptor[String](StringNodeDescriptor,
                                       DiHyper.descriptor[String](Some(new HyperEdgeSerializer))) 
    val exp = new Export[String,DiHyperEdge](g, descr)
    val n = exp.jsonASTNodes
    val e = exp.jsonASTEdges
    val p = exp.jsonAST(n ++ e)
    val t = exp.jsonText(p)
    factory.fromJson[String,DiHyperEdge](t, descr) should be (g)
  }
}
/** Turns a JString into a String thus allowing to omit parameter names.
 *  The standard lift.json serializer expects a JField;
 *  Serializers must be defined at top level. */
private final class StringSerializer extends CustomSerializer[String]( formats => (
  { case JString(label) => label
  },
  { case s: String => JString(s)
  }))
