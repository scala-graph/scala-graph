package scalax.collection
package io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalax.collection.OuterImplicits.*
import scalax.collection.generic.{AbstractHyperEdge, AbstractUnDiEdge, AnyHyperEdge}
import scalax.collection.edges.*
import scalax.collection.immutable.{Graph, TypedGraphFactory}

class NonLabeledWithNodeReferencesSpec extends GraphCodecSpecBase:

  def `verbose Airports connected with edges of type DiEdge`(): Unit = {
    import NonLabeledWithNodeReferencesSpec.Airport
    import Airport.*

    type G = Graph[Airport, DiEdge[Airport]]
    val graph: G = Graph(london ~> amsterdam, amsterdam ~> london)
    val json     =
      s"""{
        "nodes": [
          {"code": "LHR", "name": {"EN":"$london_EN",    "GER": "$london_DE"  }},
          {"code": "AMS", "name": {"EN":"$amsterdam_EN", "ES":  "$amsterdam_ES"}}
        ],
        "edges": [
          { "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "AMS" },
          { "edgeT": "DiEdge", "sourceId": "AMS", "targetId": "LHR" }
        ]
      }""".filterNot(_.isWhitespace)

    given nodeCodec: JsonValueCodec[Airport]                          = JsonCodecMaker.make
    given idCodec: JsonValueCodec[String]                             = JsonCodecMaker.make
    given edgeCodec: JsonValueCodec[DiEdgeWithNodeReferences[String]] = JsonCodecMaker.make
    given graphCodec: JsonValueCodec[G]                               =
      GraphCodec.withNodeReferences(
        _.code,
        DiEdgeWithNodeReferences.apply,
        factory = Graph.from(_, _)(_),
        smallGraphConfig,
        onJsonNullFail[G],
        edgeFactory = Some(DiEdgeWithNodeReferences.diEdgeFactory[Airport])
      )

    writeToString(graph) shouldBe json
    readFromString[G](json) shouldBe graph
  }

  def `mixed graph of verbose Airports and an ADT of edges`(): Unit = {
    import NonLabeledWithNodeReferencesSpec.Airport
    import NonLabeledWithNodeReferencesSpec.Mixed.*
    import Airport.*

    val graph: Airports = Airports.from(
      NonStop(london, amsterdam) ::
        NonStop(london, newYork) ::
        Partnership(Several(london, amsterdam, newYork)) :: Nil
    )
    val json =
      s"""{
        "nodes": [
          {"code": "LHR", "name": {"EN":"$london_EN",    "GER": "$london_DE"   }},
          {"code": "JFK", "name": {"EN":"$newYork_EN",   "NL":  "$newYork_NL"  }},
          {"code": "AMS", "name": {"EN":"$amsterdam_EN", "ES":  "$amsterdam_ES"}}
        ],
        "edges": [
          { "type": "UnDiEdgeWithNodeReferences",  "edgeT": "NonStop", "id1": "LHR", "id2": "AMS" },
          { "type": "UnDiEdgeWithNodeReferences",  "edgeT": "NonStop", "id1": "LHR", "id2": "JFK" },
          { "type": "HyperEdgeWithNodeReferences", "edgeT": "Partnership", "endIds":{"head":"LHR","_2":"AMS","more":["JFK"]} }
        ]
      }""".filterNot(_.isWhitespace)

    given nodeCodec: JsonValueCodec[Airport]                                = JsonCodecMaker.make
    given idCodec: JsonValueCodec[String]                                   = JsonCodecMaker.make
    given edgeCodec: JsonValueCodec[AnyHyperEdgeWithNodeReferences[String]] = JsonCodecMaker.make
    given graphCodec: JsonValueCodec[Airports]                              =
      GraphCodec.withNodeReferences(
        _.code,
        HyperEdgeWithNodeReferences.apply,
        factory = Graph.from(_, _)(_),
        smallGraphConfig,
        onJsonNullFail[Airports],
        edgeFactory = Some { case ("NonStop", a1, a2) =>
          NonStop(a1, a2)
        },
        hyperEdgeFactory = Some { case ("Partnership", ends) =>
          Partnership(ends)
        }
      )

    writeToString(graph) shouldBe json
    readFromString[Airports](json) shouldBe graph
  }

private object NonLabeledWithNodeReferencesSpec:

  case class Airport(code: String, name: Map[String, String])
  object Airport:
    val (london_EN, london_DE) = ("London_Heathrow_Airport", "Flughafen_London_Heathrow")
    val london                 = Airport(
      "LHR",
      Map(
        "EN"  -> london_EN,
        "GER" -> london_DE
      )
    )
    val (amsterdam_EN, amsterdam_ES) = ("LAmsterdam_Airport_Schiphol", "Aeropuerto_de_Ámsterdam-Schiphol")
    val amsterdam                    = Airport(
      "AMS",
      Map(
        "EN" -> amsterdam_EN,
        "ES" -> amsterdam_ES
      )
    )
    val (newYork_EN, newYork_NL) = ("John_F._Kennedy_International_Airport", "Luchthaven_John_F._Kennedy")
    val newYork                  = Airport(
      "JFK",
      Map(
        "EN" -> newYork_EN,
        "NL" -> newYork_NL
      )
    )

  object Mixed:
    type Airports = Graph[Airport, Relation]
    object Airports extends TypedGraphFactory[Airport, Relation]

    sealed trait Relation                                    extends AnyHyperEdge[Airport]
    case class NonStop(airport1: Airport, airport2: Airport) extends AbstractUnDiEdge(airport1, airport2) with Relation
    case class Partnership(partners: Several[Airport])       extends AbstractHyperEdge(partners) with Relation
