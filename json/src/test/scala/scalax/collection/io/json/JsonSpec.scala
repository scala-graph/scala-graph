package scalax.collection
package io.json

import net.liftweb.json._

import scalax.collection.OuterImplicits._
import scalax.collection.generic.{AnyEdge, Edge, GenericGraphCoreFactory}
import scalax.collection.edges._
import serializer._
import imp._
import imp.Parser.{parse => graphParse}
import descriptor._
import descriptor.predefined._
import descriptor.Defaults._
import exp.Export

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.Suites

class JsonSpec
    extends Suites(
      new Json[immutable.Graph](immutable.Graph),
      new Json[mutable.Graph](mutable.Graph)
    )

class Json[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  object `JSON import/export of` {
    import scalax.collection.edges.multilabeled._
    object `mixed graphs` {
      private object Fixture {
        val jsonText = """
          { "nodes" : [["A"], ["B"], ["C"], ["X"], ["Y"]],
            "edges": [
              ["A", "B"],
              ["B", "C"]
            ],
            "edges": { "MultiWDiEdge": [
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
              MultiWDi.descriptor[String]()
            )
          )
        val graph = factory[String, AnyEdge](
          "A" ~ "B",
          "A" ~> "B" %% 3,
          "B" ~ "C",
          "B" ~> "C" %% 4,
          "X" ~ "Y",
          "Y" ~ "A"
        )
      }
      import Fixture._

      def `on parsing`(): Unit = {
        val lists = graphParse(jsonText, descriptor).toList

        lists should have size 4
        lists.head match {
          case NodeList(nodeTypeId, _nodes) =>
            nodeTypeId should be(defaultId)
            _nodes should have size 5
            val nodes = _nodes.toList
            nodes.head match {
              case JArray(fields) =>
                fields.toString should be("List(JString(A))")
              case _ => fail()
            }
            nodes(4) match {
              case JArray(fields) =>
                fields.toString should be("List(JString(Y))")
              case _ => fail()
            }
          case _ => fail()
        }
        lists(1) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be(defaultId)
            _edges should have size 2
            val edges = _edges.toList
            edges.head match {
              case JArray(fields) =>
                fields.toString should be("List(JString(A), JString(B))")
              case _ => fail()
            }
            edges(1) match {
              case JArray(fields) =>
                fields.toString should be("List(JString(B), JString(C))")
              case _ => fail()
            }
          case _ => fail()
        }
        lists(2) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be("MultiWDiEdge")
            _edges should have size 2
            val edges = _edges.toList
            edges.head match {
              case JArray(fields) =>
                fields.toString should be("List(JString(A), JString(B), JDouble(3.0))")
              case _ => fail()
            }
            edges(1) match {
              case JArray(fields) =>
                fields.toString should be("List(JString(B), JString(C), JDouble(4.0))")
              case _ => fail()
            }
          case _ => fail()
        }
        lists(3) match {
          case EdgeList(edgeTypeId, _edges) =>
            edgeTypeId should be("UnDiEdge")
            _edges should have size 2
            val edges = _edges.toList
            edges.head match {
              case JObject(fields) =>
                fields.toString should be("List(JField(n1,JString(X)), JField(n2,JString(Y)))")
              case _ => fail()
            }
            edges(1) match {
              case JObject(fields) =>
                fields.toString should be("List(JField(n1,JString(Y)), JField(n2,JString(A)))")
              case _ => fail()
            }
            ()
          case _ => fail()
        }
      }

      def `on import`(): Unit =
        factory.fromJson[String, UnDiEdge[String]](jsonText, descriptor) shouldBe graph

      def `on reimport`(): Unit = {
        val g = factory.fromJson[String, UnDiEdge[String]](jsonText, descriptor)
        factory.fromJson[String, UnDiEdge[String]](g.toJson(descriptor), descriptor) shouldBe g
      }
    }

    object `graphs with weighted edges` {
      import edges.labeled._

      private object Fixture {
        val jsonText = """
          { "nodes" : [["A"], ["B"]],
            "edges": [
              ["A", "B", 100.0],
              ["B", "A", 200.0]
            ]
          }""".filterNot(_.isWhitespace)
        val descriptor = new Descriptor[String](
          StringNodeDescriptor,
          WDi.descriptor[String]()
        )
        val graph = factory("A" ~> "B" % 100, "B" ~> "A" % 200)
      }
      import Fixture._

      def `on import`(): Unit =
        factory.fromJson[String, WDiEdge[String]](jsonText, descriptor) shouldBe graph

      def `on reimport`(): Unit = {
        val g = factory.fromJson[String, WDiEdge[String]](jsonText, descriptor)
        factory.fromJson[String, WDiEdge[String]](g.toJson(descriptor), descriptor) shouldBe g
      }
    }

    object `directed hypergraphs` {
      import hyperedges._

      object `using default edge descriptors` {
        def `on exporting`(): Unit = {
          val g = factory(OneOrMore("B") ~~> OneOrMore("A", "C"))
          val descriptor =
            new Descriptor[String](
              StringNodeDescriptor,
              DiHyper.descriptor[String]()
            )
          val exp = new Export[String, DiHyperEdge[String]](g, descriptor)
          val n   = exp.jsonASTNodes
          val e   = exp.jsonASTEdges
          val p   = exp.jsonAST(List(n, e))
          val t   = exp.jsonText(p)
          factory.fromJson[String, DiHyperEdge[String]](t, descriptor) shouldBe g
        }
      }
    }
  }
}
