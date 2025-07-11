package scalax.collection
package io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OneOrMore.one
import scalax.collection.OuterImplicits.*
import scalax.collection.generic.{AbstractUnDiEdge, AnyEdge, MultiEdge}
import scalax.collection.edges.*
import scalax.collection.immutable.{Graph, TypedGraphFactory}

class GraphCodecSpec extends RefSpec with Matchers:

  private def onJsonNullFail[A] = null.asInstanceOf[A]

  private val smallGraphConfig = GraphCodec.graphConfig(orderHint = 32, degreeHint = 8)

  object `graph with non-labeled edges, JSON with embedded nodes`:

    def `empty Graph`: Unit = {
      type G = Graph[Int, DiEdge[Int]]
      val graph: G = Graph.empty
      val json     =
        """{
          "nodes": [],
          "edges": []
        }""".filterNot(_.isWhitespace)

      given nodeCodec: JsonValueCodec[Int]                      = JsonCodecMaker.make
      given edgeCodec: JsonValueCodec[DiEdge[Int]]              = JsonCodecMaker.make
      given graphCodec: JsonValueCodec[Graph[Int, DiEdge[Int]]] =
        GraphCodec.withEmbeddedNodes(
          onJsonNullFail[G],
          Graph.from(_, _)(_),
          smallGraphConfig
        )

      writeToString(graph) shouldBe json
      readFromString[G](json) shouldBe graph
    }

    def `Graph with nodes of primitive type`(): Unit = {
      val graph = Graph(1 ~> 2, 2 ~> 3)
      val json  =
        """{
          "nodes": [1, 2, 3],
          "edges": [
            {"source": 1, "target": 2 },
            {"source": 2, "target": 3 }
          ]
        }""".filterNot(_.isWhitespace)

      type G = Graph[Int, DiEdge[Int]]
      given nodeCodec: JsonValueCodec[Int]                      = JsonCodecMaker.make
      given edgeCodec: JsonValueCodec[DiEdge[Int]]              = JsonCodecMaker.make
      given graphCodec: JsonValueCodec[Graph[Int, DiEdge[Int]]] =
        GraphCodec.withEmbeddedNodes(
          onJsonNullFail[G],
          Graph.from(_, _)(_),
          smallGraphConfig
        )
      writeToString(graph) shouldBe json
      readFromString[G](json) shouldBe graph
    }

    def `mixed Graph with ADT of edges`(): Unit = {
      import GraphCodecSpec.*
      val kate = Person("Kate")
      val john = Person("John")
      val mike = Person("Mike")

      val graph: People = People.from(
        Relatives(kate, john) ::
          Friends(kate, john) ::
          Neighbors(john, mike) :: Nil
      )

      def jsonOf(p: Person) = s"""{"name":"${p.name}"}"""
      val kateJson          = jsonOf(kate)
      val johnJson          = jsonOf(john)
      val mikeJson          = jsonOf(mike)
      val json              =
        s"""{
          "nodes": [$kateJson, $johnJson, $mikeJson],
          "edges": [
            {"type": "$Relatives", "personA": $kateJson, "personB": $johnJson },
            {"type": "$Friends", "personA": $kateJson, "personB": $johnJson },
            {"type": "$Neighbors", "personA": $johnJson, "personB": $mikeJson }
          ]
        }""".filterNot(_.isWhitespace)

      given nodeCodec: JsonValueCodec[Person]   = JsonCodecMaker.make
      given edgeCodec: JsonValueCodec[Relation] = JsonCodecMaker.make
      given graphCodec: JsonValueCodec[People]  =
        GraphCodec.withEmbeddedNodes(
          onJsonNullFail[People],
          Graph.from(_, _)(_),
          smallGraphConfig
        )

      writeToString(graph) shouldBe json
      readFromString[People](json) shouldBe graph
    }

    def `mixed Graph with non-ADT edges`: Unit = {
      val graph = Graph[String, AnyEdge](
        "A" ~ "B",
        "B" ~> "C",
        "X"
      )
      val json =
        """{
          "nodes": ["A", "B", "C", "X"],
          "edges": [
            { "type": "UnDiEdge", {"source": "A", "target": "B" }},
            { "type": "DiEdge", {"source": "B", "target": "C" }}
          ]
        }""".filterNot(_.isWhitespace)

      type G = Graph[String, AnyEdge[String]]
      given nodeCodec: JsonValueCodec[String]             = JsonCodecMaker.make
      given anyEdgeCodec: JsonValueCodec[AnyEdge[String]] =
        given unDiEdgeCodec: JsonValueCodec[UnDiEdge[String]] = JsonCodecMaker.make
        given diEdgeCodec: JsonValueCodec[DiEdge[String]]     = JsonCodecMaker.make
        EdgeCodec.makePolymorphicWithEmbeddedNodes[AnyEdge[String], (UnDiEdge[String], DiEdge[String])](
          onJsonNullFail[AnyEdge[String]]
        )
      given graphCodec: JsonValueCodec[G] =
        GraphCodec.withEmbeddedNodes(
          onJsonNullFail[Graph[String, AnyEdge[String]]],
          Graph.from(_, _)(_),
          smallGraphConfig
        )

      writeToString(graph) shouldBe json
      readFromString[G](json) shouldBe graph
    }
end GraphCodecSpec

private object GraphCodecSpec:
  type People = Graph[Person, Relation]
  object People extends TypedGraphFactory[Person, Relation]

  case class Person(name: String)

  sealed abstract class Relation(personA: Person, personB: Person)
      extends AbstractUnDiEdge(personA, personB)
      with MultiEdge {
    def extendKeyBy: OneOrMore[Any] = one(getClass.getSimpleName)
  }

  case class Relatives(personA: Person, personB: Person) extends Relation(personA, personB)
  case class Friends(personA: Person, personB: Person)   extends Relation(personA, personB)
  case class Neighbors(personA: Person, personB: Person) extends Relation(personA, personB)
