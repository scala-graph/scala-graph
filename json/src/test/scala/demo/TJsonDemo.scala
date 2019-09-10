package demo

import scalax.collection._
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.{Di, DiHyper}

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
class TJsonDemoTest extends RefSpec with Matchers {
  val (programming, inDepth) = (
    Book("Programming in Scala", "978-0-9815316-2-5"),
    Book("Scala in Depth", "978-1-9351827-0-2")
  )
  val (martin, lex, bill, josh) = (
    Author("Odersky", "Martin"),
    Author("Spoon", "Lex"),
    Author("Venners", "Bill"),
    Author("Suereth", "Joshua D.")
  )
  /* Directed edges/hyperedges denote book-author-relationships.
   */
  val library = Graph[Library, HyperEdge](
    programming ~> martin ~> lex ~> bill,
    inDepth ~> josh
  )
  val bookDescriptor = new NodeDescriptor[Book](typeId = "Books") {
    def id(node: Any) = node match {
      case Book(_, isbn) => isbn
    }
  }
  val authorDescriptor = new NodeDescriptor[Author](typeId = "Authors") {
    def id(node: Any) = node match {
      case Author(surName, firstName) => "" + surName(0) + firstName(0)
    }
  }

  object `When calling JSON import/export ` {
    def `it is required that proper node descriptors are passed` {
      val quickJson = new Descriptor[Library](
        defaultNodeDescriptor = authorDescriptor,
        defaultEdgeDescriptor = DiHyper.descriptor[Library]()
      )
      val caught =
        intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
          library.toJson(quickJson)
        }
      caught.msg should be("""No 'NodeDescriptor' capable of processing type "demo.Book" found.""")
    }
    def `it is required that proper edge descriptors are passed` {
      val quickJson = new Descriptor[Library](
        defaultNodeDescriptor = authorDescriptor,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](),
        namedNodeDescriptors = Seq(bookDescriptor)
      )
      val caught =
        intercept[scalax.collection.io.json.error.JsonGraphError.JsonGraphException] {
          library.toJson(quickJson)
        }
      caught.msg should be(
        """No 'EdgeDescriptor' capable of processing type "scalax.collection.GraphEdge$DiEdge" found."""
      )
    }
  }

  object `When choosing JSON format based on JSON objects` {
    private object Named {
      val descriptor = new Descriptor[Library](
        defaultNodeDescriptor = authorDescriptor,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](),
        namedNodeDescriptors = Seq(bookDescriptor),
        namedEdgeDescriptors = Seq(Di.descriptor[Library]())
      )
    }
    def `export works fine` {
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
    def `importing the exported JSON yields an equal graph` {
      val expLibrary = library.toJson(Named.descriptor)
      Graph.fromJson[Library, HyperEdge](expLibrary, Named.descriptor) should equal(library)
    }
  }

  object `When choosing JSON format based on JSON arrays to save space` {

    private object PositionedNodeDescriptor {
      import net.liftweb.json._
      final class AuthorSerializer
          extends CustomSerializer[Author](formats =>
            ({
              case JArray(JString(surName) :: JString(firstName) :: Nil) =>
                Author(surName, firstName)
            }, {
              case Author(surName, firstName) =>
                JArray(JString(surName) :: JString(firstName) :: Nil)
            }))
      val author = new NodeDescriptor[Author](typeId = "Authors", customSerializers = Seq(new AuthorSerializer)) {
        def id(node: Any) = node match {
          case Author(surName, firstName) => "" + surName(0) + firstName(0)
        }
      }
      final class BookSerializer
          extends CustomSerializer[Book](formats =>
            ({ case JArray(JString(title) :: JString(isbn) :: Nil) => Book(title, isbn) }, {
              case Book(title, isbn)                               => JArray(JString(title) :: JString(isbn) :: Nil)
            }))
      val book = new NodeDescriptor[Book](typeId = "Books", customSerializers = Seq(new BookSerializer)) {
        def id(node: Any) = node match {
          case Book(_, isbn) => isbn
        }
      }
    }
    private object Positioned {
      import scalax.collection.io.json.serializer.{EdgeSerializer, HyperEdgeSerializer}
      val descriptor = new Descriptor[Library](
        defaultNodeDescriptor = PositionedNodeDescriptor.author,
        defaultEdgeDescriptor = DiHyper.descriptor[Library](Some(new HyperEdgeSerializer)),
        namedNodeDescriptors = Seq(PositionedNodeDescriptor.book),
        namedEdgeDescriptors = Seq(Di.descriptor[Library](Some(new EdgeSerializer)))
      )
    }
    def `export works fine` {
      val exported = library.toJson(Positioned.descriptor)

      import net.liftweb.json._
      val pretty = prettyRender(JsonParser.parse(exported))
      // println(pretty)
      /*{
          "nodes":{
            "Books":[["Scala in Depth","978-1-9351827-0-2"],["Programming in Scala","978-0-9815316-2-5"]],
            "Authors":[["Odersky","Martin"],["Spoon","Lex"],["Venners","Bill"],["Suereth","Joshua D."]]
          },
          "edges":{
            "DiHyperEdge":[["978-0-9815316-2-5","OM","SL","VB"], "Bag"],
            "DiEdge":[["978-1-9351827-0-2","SJ"]]
          }
        }
     */
    }
    def `importing the exported JSON yields an equal graph` {
      val expLibrary = library.toJson(Positioned.descriptor)
      Graph.fromJson[Library, HyperEdge](expLibrary, Positioned.descriptor) should equal(library)
    }
  }
}
// --------------------------------- node types of academic library application
sealed trait Library
case class Book(title: String, isbn: String)          extends Library
case class Author(surName: String, firstName: String) extends Library
