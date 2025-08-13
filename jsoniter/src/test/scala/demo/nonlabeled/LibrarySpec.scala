package demo.nonlabeled

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OneOrMore
import scalax.collection.OuterImplicits.*
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.generic.AnyDiHyperEdge
import scalax.collection.hyperedges.*
import scalax.collection.immutable.Graph
import scalax.collection.io.jsoniter.{GraphCodec => _, *}
import scalax.collection.io.jsoniter.nonlabeled.*

class LibrarySpec extends RefSpec with Matchers with GraphCodecSpecBase:
  import util.sameAs

  // nodes of type Book
  private val (programming, inDepth) = (
    Book("Programming in Scala", "978-0-9815316-2-5"),
    Book("Scala in Depth", "978-1-9351827-0-2")
  )

  // nodes of type Author
  private val (martin, lex, bill, josh) = (
    Author("Odersky", "Martin"),
    Author("Spoon", "Lex"),
    Author("Venners", "Bill"),
    Author("Suereth", "Joshua D.")
  )

  private type Library = Graph[Node, AnyDiHyperEdge[Node]]

  // directed edges or hyperedges to denote book-author relationships
  private val library: Library = Graph(
    OneOrMore(programming) ~~> OneOrMore(martin, lex, bill),
    inDepth ~> josh
  )

  object `When choosing a codec with node references`:

    import AnyDiHyperEdgeWithNodeReferences.compactClassNames
    given nodeCodec: JsonValueCodec[Node]                                     = JsonCodecMaker.make
    given idCodec: JsonValueCodec[String]                                     = JsonCodecMaker.make
    given edgeCodec: JsonValueCodec[AnyDiHyperEdgeWithNodeReferences[String]] = JsonCodecMaker.make(compactClassNames)
    given graphCodec: JsonValueCodec[Library]                                 =
      GraphCodec.withNodeReferences(
        {
          case Book(_, isbn)              => isbn
          case Author(surName, firstName) => "" + surName.head + firstName.head
        },
        DiHyperEdgeWithNodeReferences.apply,
        Graph.from(_, _)(_),
        smallGraphConfig,
        onJsonNullFail[Library],
        edgeFactory = Some(DiEdgeWithNodeReferences.diEdgeFactory[Node]),
        diHyperEdgeFactory = Some(DiHyperEdgeWithNodeReferences.diHyperEdgeFactory[Node])
      )

    val expectedJson =
      s"""{
        "nodes": [
          { "type": "Book", "title":"Scala in Depth",       "isbn":"978-1-9351827-0-2" },
          { "type": "Book", "title":"Programming in Scala", "isbn":"978-0-9815316-2-5" },
          { "type": "Author", "surName":"Odersky", "firstName":"Martin"    },
          { "type": "Author", "surName":"Spoon",   "firstName":"Lex"       },
          { "type": "Author", "surName":"Venners", "firstName":"Bill"      },
          { "type": "Author", "surName":"Suereth", "firstName":"Joshua D." }
        ],
        "edges": [
          { "type": "DiHyperR", "edgeT": "DiHyperEdge", "sourceIds": ["978-0-9815316-2-5"], "targetIds": ["OM","SL","VB"] },
          { "type": "DiR",      "edgeT": "DiEdge",      "sourceId":   "978-1-9351827-0-2",  "targetId":   "SJ"            }
        ]
      }"""

    def `toJson yields the expected JSON`(): Unit =
      library.toJson shouldBe sameAs(expectedJson)

    def `toGraph yields the original graph`(): Unit =
      expectedJson.toGraph[Library] shouldBe library

// --------------------------------- node types of an academic library graph
sealed trait Node
case class Book(title: String, isbn: String)          extends Node
case class Author(surName: String, firstName: String) extends Node
