package scalax.collection.io.jsoniter
package labeled

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalax.collection.OuterImplicits.*
import scalax.collection.OneOrMore
import scalax.collection.generic.{AbstractDiEdge, AnyDiEdge, LDiEdgeToString, MultiEdge, MultiLEdgeToString}
import scalax.collection.immutable.{Graph, TypedGraphFactory}

class WithNodeReferencesSpec extends GraphCodecSpecBase:
  import util.sameAs

  def `verbose Airports connected with labeled edges of type Flight`(): Unit = {
    import WithNodeReferencesSpec.{Airport, flights}
    import Airport.*
    import flights.*

    val graph: Flights = Flights.from(
      Flight(london, amsterdam, "KL 1722", 40) ::
        Flight(london, newYork, "UA 921", 340) ::
        Nil
    )
    val json =
      s"""{
        "nodes": [
          {"code": "AMS", "name": {"EN": "$amsterdamEn", "ES":  "$amsterdamEs"}},
          {"code": "LHR", "name": {"EN": "$londonEn",    "GER": "$londonDe"   }},
          {"code": "JFK", "name": {"EN": "$newYorkEn",   "NL":  "$newYorkNl"  }}
        ],
        "edges": [
          { "edgeT": "Flight", "sourceId": "LHR", "targetId": "AMS", "label": ["KL 1722",  40] },
          { "edgeT": "Flight", "sourceId": "LHR", "targetId": "JFK", "label": ["UA 921",  340] }
        ]
      }"""

    type L = (String, Int)
    given nodeCodec: JsonValueCodec[Airport]                             = JsonCodecMaker.make
    given idCodec: JsonValueCodec[String]                                = JsonCodecMaker.make
    given labelCodec: JsonValueCodec[L]                                  = JsonCodecMaker.make
    given edgeCodec: JsonValueCodec[DiEdgeWithNodeReferences[String, L]] = JsonCodecMaker.make
    given graphCodec: JsonValueCodec[Flights]                            =
      GraphCodec.withNodeReferences(
        _.code,
        { case Flight(_, _, flightNo, minutes) => (flightNo, minutes) },
        DiEdgeWithNodeReferences.apply,
        factory = Flights.from(_, _)(_),
        smallGraphConfig,
        onJsonNullFail[Flights],
        edgeFactory = Some { case (edgeT: String, from: Airport, to: Airport, label: L) =>
          Flight(from, to, label._1, label._2)
        }
      )

    writeToString(graph) shouldBe sameAs(json)
    readFromString[Flights](json) shouldBe graph
  }

  def `verbose Airports connected by an ADT of labeled edges`(): Unit = {
    import WithNodeReferencesSpec.Airport
    import Airport.*
    import WithNodeReferencesSpec.mixed.*

    val graph: Airports = Airports.from(
      NonStop(london, amsterdam, "BA") ::
        NonStop(london, newYork, "BA") ::
        Connecting(amsterdam, newYork, "LHR", "BA", "UL") :: Nil
    )
    val json =
      s"""{
        "nodes": [
          {"code": "AMS", "name": {"EN": "$amsterdamEn", "ES":  "$amsterdamEs"}},
          {"code": "LHR", "name": {"EN": "$londonEn",    "GER": "$londonDe"   }},
          {"code": "JFK", "name": {"EN": "$newYorkEn",   "NL":  "$newYorkNl"  }}
        ],
        "edges": [
          { "edgeT": "NonStop",    "sourceId": "LHR", "targetId": "AMS",
            "label": { "type":"NonStopLabel", "airline": "BA" }
          },
          { "edgeT": "NonStop",    "sourceId": "LHR", "targetId": "JFK",
            "label": { "type":"NonStopLabel", "airline": "BA" }
          },
          { "edgeT": "Connecting", "sourceId": "AMS", "targetId": "JFK",
            "label": { "type":"ConnectingLabel", "via": "LHR", "airline1": "BA", "airline2": "UL"}
          }
        ]
      }"""

    import AnyEdgeWithNodeReferences.compactClassNames
    given nodeCodec: JsonValueCodec[Airport] = JsonCodecMaker.make
    given idCodec: JsonValueCodec[String]    = JsonCodecMaker.make

    sealed trait Label
    case class NonStopLabel(airline: String)                                    extends Label
    case class ConnectingLabel(via: String, airline1: String, airline2: String) extends Label

    given labelCodec: JsonValueCodec[Label]                                  = JsonCodecMaker.make
    given edgeCodec: JsonValueCodec[DiEdgeWithNodeReferences[String, Label]] = JsonCodecMaker.make(compactClassNames)
    given graphCodec: JsonValueCodec[Airports]                               =
      GraphCodec.withNodeReferences(
        _.code,
        {
          case NonStop(_, _, airline)        => NonStopLabel(airline)
          case Connecting(_, _, via, a1, a2) => ConnectingLabel(via, a1, a2)
        },
        DiEdgeWithNodeReferences.apply,
        factory = Graph.from(_, _)(_),
        smallGraphConfig,
        onJsonNullFail[Airports],
        edgeFactory = Some {
          case ("NonStop", from, to, NonStopLabel(airline))           => NonStop(from, to, airline)
          case ("Connecting", from, to, ConnectingLabel(via, a1, a2)) => Connecting(from, to, via, a1, a2)
        }
      )

    writeToString(graph) shouldBe sameAs(json)
    readFromString[Airports](json) shouldBe graph
  }

private object WithNodeReferencesSpec:

  type Airport = nonlabeled.WithNodeReferencesSpec.Airport
  val Airport = nonlabeled.WithNodeReferencesSpec.Airport

  object flights:
    type Flights = Graph[Airport, Flight]
    object Flights extends TypedGraphFactory[Airport, Flight]

    case class Flight(
        departure: Airport,
        destination: Airport,
        flightNo: String,
        durationMinutes: Int
    ) extends AbstractDiEdge[Airport](departure, destination)
        with MultiEdge
        with LDiEdgeToString
        with MultiLEdgeToString:
      override def extendKeyBy: OneOrMore[String] = OneOrMore(flightNo)

      override protected def labelToString: String = s"($flightNo, $durationMinutes)"

  object mixed:
    type Airports = Graph[Airport, Relation]
    object Airports extends TypedGraphFactory[Airport, Relation]

    sealed trait Relation extends AnyDiEdge[Airport]

    case class NonStop(from: Airport, to: Airport, airline: String)
        extends AbstractDiEdge(from, to)
        with MultiEdge
        with Relation:
      override def extendKeyBy: OneOrMore[String] = OneOrMore(airline)

    case class Connecting(from: Airport, to: Airport, via: String, airline1: String, airline2: String)
        extends AbstractDiEdge(from, to)
        with MultiEdge
        with Relation:
      override def extendKeyBy: OneOrMore[(String, String, String)] =
        OneOrMore((via, airline1, airline2))
