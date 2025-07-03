package demo

import scalax.collection._
import scalax.collection.OuterImplicits._
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.generic.AnyDiHyperEdge
import scalax.collection.hyperedges._
import scalax.collection.immutable.Graph
import scalax.collection.io.json._

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class JsonDemoSpec extends RefSpec with Matchers {

  import scalax.collection.io.json.descriptor.predefined.{Di, DiHyper}

  // nodes of type Book
  val (programming, inDepth) = (
    Book("Programming in Scala", "978-0-9815316-2-5"),
    Book("Scala in Depth", "978-1-9351827-0-2")
  )

  // nodes of type Author
  val (martin, lex, bill, josh) = (
    Author("Odersky", "Martin"),
    Author("Spoon", "Lex"),
    Author("Venners", "Bill"),
    Author("Suereth", "Joshua D.")
  )

  // directed edges/hyperedges to denote book-author relationships
  val library = Graph[Library, AnyDiHyperEdge](
    OneOrMore(programming) ~~> OneOrMore(martin, lex, bill),
    inDepth ~> josh
  )

  val bookDescriptor = new NodeDescriptor[Book](typeId = "Books") {
    def id[B >: Book](node: B): String = node match {
      case Book(_, isbn) => isbn
    }
  }
  val authorDescriptor = new NodeDescriptor[Author](typeId = "Authors") {
    def id[B >: Author](node: B): String = node match {
      case Author(surName, firstName) => "" + surName.head + firstName.head
    }
  }

  object `JSON import/export requires that` {
    def `proper node descriptors are passed`(): Unit = {
      val quickJson = Descriptor.simple[Library](
        nodeDescriptor = authorDescriptor,
        edgeDescriptor = DiHyper.descriptor[Library]()
      )
      val caught =
        intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
          library.toJson(quickJson)
        }
      caught.msg should be("""No 'NodeDescriptor' capable of processing type "demo.Book" found.""")
    }
    def `proper edge descriptors are passed`(): Unit = {
      val quickJson =
        Descriptor[Library](authorDescriptor, bookDescriptor)(DiHyper.descriptor[Library]())()
      val caught =
        intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
          library.toJson(quickJson)
        }
      caught.msg should be(
        """No 'EdgeDescriptor' capable of processing type "scalax.collection.edges.DiEdge" found."""
      )
    }
  }

  object `When choosing JSON format based on JSON objects` {
    private object Named {
      val descriptor =
        Descriptor[Library](
          authorDescriptor,
          bookDescriptor
        )(DiHyper.descriptor[Library](), Di.descriptor[Library]())()
    }

    def `export works fine`(): Unit = {
      val exported = library.toJson(Named.descriptor)

      import net.liftweb.json._
      val pretty = prettyRender(JsonParser.parse(exported))
      /*{
          "nodes":{
            "Books":[{
              "title":"Scala in Depth",
              "isbn":"978-1-9351827-0-2"
            },{
              "title":"Programming in Scala",
              "isbn":"978-0-9815316-2-5"
            }],
            "Authors":[{
              "surName":"Odersky",
              "firstName":"Martin"
            },{
              "surName":"Spoon",
              "firstName":"Lex"
            },{
              "surName":"Venners",
              "firstName":"Bill"
            },{
              "surName":"Suereth",
              "firstName":"Joshua D."
            }]
          },
          "edges":{
            "DiHyperEdge":[{
              "nodeIds":["978-0-9815316-2-5","OM","SL","VB"]
            }],
            "DiEdge":[{
              "n1":"978-1-9351827-0-2",
              "n2":"SJ"
            }]
          }
        }*/
    }
    def `importing the exported JSON yields an equal graph`(): Unit = {
      val expLibrary = library.toJson(Named.descriptor)
      Graph.fromJson[Library, HyperEdge[Library]](expLibrary, Named.descriptor) should equal(library)
    }
  }

  object `When choosing JSON format based on JSON arrays to save space` {

    private object PositionedNodeDescriptor {
      import net.liftweb.json._

      private class AuthorSerializer
          extends CustomSerializer[Author](_ =>
            (
              { case JArray(JString(surName) :: JString(firstName) :: Nil) =>
                Author(surName, firstName)
              },
              { case Author(surName, firstName) =>
                JArray(JString(surName) :: JString(firstName) :: Nil)
              }
            )
          )
      val author = new NodeDescriptor[Author](typeId = "Authors", customSerializers = Seq(new AuthorSerializer)) {
        def id[B >: Author](node: B): String = node match {
          case Author(surName, firstName) => "" + surName(0) + firstName(0)
        }
      }

      private class BookSerializer
          extends CustomSerializer[Book](_ =>
            (
              { case JArray(JString(title) :: JString(isbn) :: Nil) => Book(title, isbn) },
              { case Book(title, isbn) =>
                JArray(JString(title) :: JString(isbn) :: Nil)
              }
            )
          )
      val book = new NodeDescriptor[Book](typeId = "Books", customSerializers = Seq(new BookSerializer)) {
        def id[B >: Book](node: B): String = node match {
          case Book(_, isbn) => isbn
        }
      }
    }

    private object Positioned {
      import scalax.collection.io.json.serializer.EdgeSerializer

      val descriptor = Descriptor[Library](
        PositionedNodeDescriptor.author,
        PositionedNodeDescriptor.book
      )(
        DiHyper.descriptor[Library](),
        Di.descriptor[Library](Some(new EdgeSerializer))
      )()
    }

    def `export works fine`(): Unit = {
      val exported = library.toJson(Positioned.descriptor)

      /*
      import net.liftweb.json._
      println(prettyRender(JsonParser.parse(exported)))
       */
      /*
        {
          "nodes":{
            "Books":[["Scala in Depth","978-1-9351827-0-2"],["Programming in Scala","978-0-9815316-2-5"]],
            "Authors":[["Odersky","Martin"],["Spoon","Lex"],["Venners","Bill"],["Suereth","Joshua D."]]
          },
          "edges":{
            "DiHyperEdge":[[{ "sources":["978-0-9815316-2-5"] }, { "targets":["OM", "SL", "VB"] }]],
            "DiEdge":[["978-1-9351827-0-2","SJ"]]
          }
        }
       */
    }
    def `importing the exported JSON yields an equal graph`(): Unit = {
      val expLibrary = library.toJson(Positioned.descriptor)
      Graph.fromJson[Library, HyperEdge[Library]](expLibrary, Positioned.descriptor) should equal(library)
    }
  }
}
// --------------------------------- node types of academic library application
sealed trait Library
case class Book(title: String, isbn: String)          extends Library
case class Author(surName: String, firstName: String) extends Library
