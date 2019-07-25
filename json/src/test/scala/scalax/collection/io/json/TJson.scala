package scalax.collection.io.json

import language.higherKinds

import net.liftweb.json._

import scalax.collection._
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.edge._, scalax.collection.edge.Implicits._

import serializer._, imp._, imp.Parser.{parse => graphParse}, descriptor._, descriptor.predefined._,
descriptor.Defaults._, exp.Export

import org.scalatest._
import org.scalatest.refspec.RefSpec
class TJsonRootTest
    extends Suites(new TJsonTest[immutable.Graph](immutable.Graph), new TJsonTest[mutable.Graph](mutable.Graph))

class TJsonTest[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers {

  object `JSON import/export works fine with` {
    object `mixed graphs` {
      private object FixtureMixed {
        val jsonText = """
          { "nodes" : [["A"], ["B"], ["C"], ["X"], ["Y"]],
            "edges": [
              ["A", "B"],
              ["B", "C"]
            ],
            "edges": { "WkDiEdge": [
              ["A", "B", 3.0],
              ["B", "C", 4.0]]
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
        val graph = factory("A" ~ "B", "B" ~ "C", ("A" ~%#> "B")(3), ("B" ~%#> "C")(4), "X" ~ "Y", "Y" ~ "A")
      }
      def `on parsing` {
        import FixtureMixed._
        val lists = graphParse(jsonText, descriptor).toList

        lists should have size (4)
        lists(0) match {
          case NodeList(nodeTypeId, _nodes) =>
            nodeTypeId should be(defaultId)
            _nodes should have size (5)
            val nodes = _nodes.toList
            nodes(0) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(A))")
              case _ => fail
            }
            nodes(4) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(A))")
              case _ => fail
            }
          case _ => fail
        }
        lists(1) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be(defaultId)
            _edges should have size (2)
            val edges = _edges.toList
            edges(0) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(A), JString(B))")
              case _ => fail
            }
            edges(1) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(B), JString(C))")
              case _ => fail
            }
          case _ => fail
        }
        lists(2) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be("WkDiEdge")
            _edges should have size (2)
            val edges = _edges.toList
            edges(0) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(A), JString(B), JDouble(3))")
              case _ => fail
            }
            edges(1) match {
              case JArray(fields) =>
                fields.toString should be
                ("List(JString(B), JString(C), JDouble(4))")
              case _ => fail
            }
          case _ => fail
        }
        lists(3) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be("UnDiEdge")
            _edges should have size (2)
            val edges = _edges.toList
            edges(0) match {
              case JObject(fields) =>
                fields.toString should be
                ("List(JField(v1,JString(X)), JField(v2,JString(Y)))")
              case _ => fail
            }
            edges(1) match {
              case JObject(fields) =>
                fields.toString should be
                ("List(JField(v1,JString(Y)), JField(v2,JString(A)))")
              case _ => fail
            }
            Unit
          case _ => fail
        }
      }
      def `on importing` {
        import FixtureMixed._
        val g = factory.fromJson[String, UnDiEdge](jsonText, descriptor)
        g should equal(graph)
      }
      def `on reimporting` {
        import FixtureMixed._
        val g = factory.fromJson[String, UnDiEdge](jsonText, descriptor)
        factory.fromJson[String, UnDiEdge](g.toJson(descriptor), descriptor) should equal(g)
      }
    }

    object `graphs with labeled edges` {
      private val graph = factory(("A" ~+> "B")("L1"), ("B" ~+> "A")("L2"))

      object `using default edge desriptors` {
        private object FixtureLEdge {
          val jsonText   = """
            { "nodes" : [["A"], ["B"]],
              "edges": [
                {"n1":"A", "n2":"B", "label":"L1"},
                {"n1":"B", "n2":"A", "label":"L2"}
              ]
            }""".filterNot(_.isWhitespace)
          val descriptor = new Descriptor[String](StringNodeDescriptor, LDi.descriptor[String, String](aLabel = ""))
        }
        def `on importing` {
          import FixtureLEdge._
          val g = factory.fromJson[String, LDiEdge](jsonText, descriptor)
          g should equal(graph)
        }
        def `on reimporting` {
          import FixtureLEdge._
          val g = factory.fromJson[String, LDiEdge](jsonText, descriptor)
          factory.fromJson[String, LDiEdge](g.toJson(descriptor), descriptor) should equal(g)
        }
      }
      object `using custom edge desriptors` {
        private object FixtureLEdgeCustom {
          val jsonText = """
            { "nodes" : [["A"], ["B"]],
              "edges": [
                ["A", "B", "L1"],
                ["B", "A", "L2"]
              ]
            }""".filterNot(_.isWhitespace)
          val descriptor = new Descriptor[String](
            StringNodeDescriptor,
            LDi.descriptor[String, String]("", Some(new LEdgeSerializer[String](new StringSerializer))))
        }
        def `on importing` {
          import FixtureLEdgeCustom._
          val g = factory.fromJson[String, LDiEdge](jsonText, descriptor)
          g should equal(graph)
        }
        def `on reimporting` {
          import FixtureLEdgeCustom._
          val g = factory.fromJson[String, LDiEdge](jsonText, descriptor)
          factory.fromJson[String, LDiEdge](g.toJson(descriptor), descriptor) should equal(g)
        }
      }
    }

    object `graphs with weighted labeled edges` {
      private object FixtureWLEdgeCustom {
        val jsonText = """
          { "nodes" : [["A"], ["B"]],
            "edges": [
              ["A", "B", 100.0, "CLabel-1"],
              ["B", "A", 200.0, "CLabel-2"],
            ]
          }""".filterNot(_.isWhitespace)
        val descriptor = new Descriptor[String](
          StringNodeDescriptor,
          new WLEdgeDescriptor[String, WLDiEdge, WLDiEdge.type, String](
            WLDiEdge,
            "",
            Some(new WLEdgeSerializer[String](new StringSerializer)))
        )
        val graph = factory(("A" ~%+> "B")(100, "CLabel-1"), ("B" ~%+> "A")(200, "CLabel-2"))
      }
      def `on importing` {
        import FixtureWLEdgeCustom._
        val g = factory.fromJson[String, WLDiEdge](jsonText, descriptor)
        g should equal(graph)
      }
      def `on reimporting` {
        import FixtureWLEdgeCustom._
        val g = factory.fromJson[String, WLDiEdge](jsonText, descriptor)
        factory.fromJson[String, WLDiEdge](g.toJson(descriptor), descriptor) should equal(g)
      }
    }

    private val quotedBag = s""""${Bag.toString}""""

    object `hypergraphs ` {
      object `with weighted labeled edges using custom edge desriptors` {
        def `on importing` {
          val jsonText = s"""
            { "nodes" : [["A"], ["B"], ["C"]],
              "edges": [
                [["A", "B"],      $quotedBag, 100.0, "Label-1"],
                [["B", "A", "C"], $quotedBag, 200.0, "Label-2"],
              ]
            }""".filterNot(_.isWhitespace)
          val g: CC[String, WLHyperEdge] = factory.fromJson[String, WLHyperEdge](
            jsonText,
            new Descriptor[String](
              StringNodeDescriptor,
              new WLHyperEdgeDescriptor[String, WLHyperEdge, WLHyperEdge.type, String](
                WLHyperEdge,
                "",
                Some(new WLHyperEdgeSerializer[String](new StringSerializer)))
            )
          )
          g.nodes.toSortedString() should be("NodeSet(A, B, C)")
          g.edges.toSortedString() should be("EdgeSet(A~B %100.0 'Label-1, B~A~C %200.0 'Label-2)")
        }
      }
    }
    object `directed hypergraphs` {
      object `using default edge desriptors` {
        def `on exporting` {
          val g = factory("B" ~> "A" ~> "C")
          val descr =
            new Descriptor[String](StringNodeDescriptor, DiHyper.descriptor[String](Some(new HyperEdgeSerializer)))
          val exp = new Export[String, DiHyperEdge](g, descr)
          val n   = exp.jsonASTNodes
          val e   = exp.jsonASTEdges
          val p   = exp.jsonAST(List(n, e))
          val t   = exp.jsonText(p)
          factory.fromJson[String, DiHyperEdge](t, descr) should be(g)
        }
      }
      object `using custom edge desriptors` {
        private object FixtureDiHyperEdgeCustom {
          val jsonText = s"""
            { "nodes" : [["A"], ["B"], ["C"]],
              "edges": [
                [["A", "B"],      $quotedBag],
                [["B", "A", "C"], $quotedBag],
              ]
            }""".filterNot(_.isWhitespace)
          val descriptor = new Descriptor[String](
            StringNodeDescriptor,
            new HyperEdgeDescriptor[String, DiHyperEdge, DiHyperEdge.type](DiHyperEdge, Some(new HyperEdgeSerializer))
          )
          val graph = factory[String, DiHyperEdge](DiHyperEdge("A", "B"), "B" ~> "A" ~> "C")
        }
        def `on importing` {
          import FixtureDiHyperEdgeCustom._
          val g = factory.fromJson[String, DiHyperEdge](
            jsonText,
            new Descriptor[String](
              StringNodeDescriptor,
              new HyperEdgeDescriptor[String, DiHyperEdge, DiHyperEdge.type](DiHyperEdge, Some(new HyperEdgeSerializer))
            )
          )
          g should equal(graph)
        }
        def `on reimporting` {
          import FixtureDiHyperEdgeCustom._
          val g = factory.fromJson[String, DiHyperEdge](jsonText, descriptor)
          factory.fromJson[String, DiHyperEdge](g.toJson(descriptor), descriptor) should equal(g)
        }
      }
    }
  }
}

/** Turns a JString into a String thus allowing to omit parameter names.
  *  The standard lift.json serializer expects a JField;
  *  Serializers must be defined at top level. */
final private class StringSerializer
    extends CustomSerializer[String](formats => ({ case JString(label) => label }, { case s: String => JString(s) }))
