package demo

import scalax.collection.Graph
import scalax.collection.GraphEdge._

/** Includes the examples given on [[http://www.scala-graph.org/guides/test.html
  *  Test Utilities]].
  */
object TGraphTest extends App {

  import scalax.collection.generator._

  object PersonData {
    val firstNames     = Set("Alen", "Alice", "Bob", "Jack", "Jim", "Joe", "Kate", "Leo", "Tim", "Tom").to[Vector]
    val firstNamesSize = firstNames.size

    val surnames     = Vector("Bell", "Brown", "Clark", "Cox", "King", "Lee", "Moore", "Ross", "Smith", "Wong").to[Vector]
    val surnamesSize = surnames.size

    def order          = firstNamesSize * surnamesSize / 10
    def degrees        = new NodeDegreeRange(2, order - 2)
    val maxYearOfBirth = 2010
  }

  object RG {
    // working with random graph generators -----------------------------------------------

    // obtaining generators for graphs with predefined metrics
    val predefined = RandomGraph.tinyConnectedIntDi(Graph)
    val tinyGraph  = predefined.draw // Graph[Int,DiEdge]

    // setting individual graph metrics while keeping metrics constraints in mind
    object sparse_1000_Int extends RandomGraph.IntFactory {
      val order              = 1000
      val nodeDegrees        = NodeDegreeRange(1, 10)
      override def connected = false
    }
    val randomSparse = RandomGraph[Int, UnDiEdge, Graph](Graph, sparse_1000_Int, Set(UnDiEdge))
    val sparseGraph  = randomSparse.draw // Graph[Int,UnDiEdge]

    // obtaining generators for individual graph types
    import scalax.collection.edge.LDiEdge

    case class Person(name: String, yearOfBirth: Int)
    object Person {
      import PersonData._
      private val r = new scala.util.Random

      def drawFirstName: String = firstNames(r.nextInt(firstNamesSize))
      def drawSurame: String    = surnames(r.nextInt(surnamesSize))

      def drawName: String = s"$drawFirstName, $drawSurame"

      def drawYearOfBirth = maxYearOfBirth - r.nextInt(100)
    }

    val randomMixedGraph =
      RandomGraph[Person, UnDiEdge, Graph](
        Graph,
        new RandomGraph.Metrics[Person] {
          val order           = PersonData.order
          val nodeDegrees     = PersonData.degrees
          def nodeGen: Person = Person(Person.drawName, Person.drawYearOfBirth)
        },
        Set(UnDiEdge, LDiEdge)
      )
    val mixedGraph = randomMixedGraph.draw
    /*
    println(mixedGraph)
    Graph(
        Person(Alice, Smith,1967),
        Person(Kate, Ross,1921),
        Person(Leo, Bell,2008),
        Person(Leo, Smith,1983),
        ...,
        Person(Alice, Smith,1967)~>Person(Kate, Ross,1921) 'C,
        Person(Leo, Bell,2008)~Person(Leo, Smith,1983),
        ...
    )
   */
  }

  object GG {
    import org.scalacheck.{Arbitrary, Gen}
    import Arbitrary.arbitrary
    import org.scalacheck.Prop.forAll

    // working with org.scalacheck.Arbitrary graphs ---------------------------------------

    // obtaining Arbitrary instances for graphs with predefined metrics
    type IntDiGraph = Graph[Int, DiEdge]
    implicit val arbitraryTinyGraph = GraphGen.tinyConnectedIntDi[Graph](Graph)

    val properTiny = forAll(arbitrary[IntDiGraph]) { g: IntDiGraph =>
      g.order == GraphGen.TinyInt.order
    }
    properTiny.check

    // setting individual graph metrics for Arbitrary instances
    // while keeping metrics constraints in mind
    object Sparse_1000_Int extends GraphGen.Metrics[Int] {
      val order              = 1000
      val nodeDegrees        = NodeDegreeRange(1, 10)
      def nodeGen: Gen[Int]  = Gen.choose(0, 10 * order)
      override def connected = false
    }

    type IntUnDiGraph = Graph[Int, UnDiEdge]
    implicit val arbitrarySparseGraph = Arbitrary {
      GraphGen[Int, UnDiEdge, Graph](Graph, Sparse_1000_Int, Set(UnDiEdge)).apply
    }

    val properSparse = forAll(arbitrary[IntUnDiGraph]) { g: IntUnDiGraph =>
      g.order == Sparse_1000_Int.order
    }
    properSparse.check

    // obtaining Arbitrary instances to generate individual graph types
    import scalax.collection.edge.LDiEdge

    case class Person(name: String, yearOfBirth: Int)
    object Person {
      import PersonData._

      def firstNameGen: Gen[String] = Gen.oneOf(firstNames)
      def surameGen: Gen[String]    = Gen.oneOf(surnames)

      def nameGen: Gen[String] = Gen.resultOf((firstName: String, surname: String) => s"$firstName, $surname")(
        Arbitrary(firstNameGen),
        Arbitrary(surameGen))

      def yearOfBirthGen: Gen[Int] = Gen.choose(maxYearOfBirth - 100, maxYearOfBirth)
    }

    object MixedMetrics extends GraphGen.Metrics[Person] {
      val order       = PersonData.order
      val nodeDegrees = PersonData.degrees
      def nodeGen: Gen[Person] = Gen.resultOf((name: String, year: Int) => Person(name, year))(
        Arbitrary(Person.nameGen),
        Arbitrary(Person.yearOfBirthGen))
    }

    type Mixed = Graph[Person, UnDiEdge]
    implicit val arbitraryMixedGraph = Arbitrary {
      GraphGen[Person, UnDiEdge, Graph](Graph, MixedMetrics, Set(UnDiEdge, LDiEdge)).apply
    }

    val properMixedGraph = forAll(arbitrary[Mixed]) { g: Mixed =>
      g.order == MixedMetrics.order
    }
    properMixedGraph.check
    println(arbitraryMixedGraph.arbitrary.sample)

    // Integrating with ScalaTest, limiting the minimum # of successful test
    import org.scalatest.Matchers
    import org.scalatest.prop.PropertyChecks
    import org.scalatest.refspec.RefSpec

    class TGraphGenTest extends RefSpec with Matchers with PropertyChecks {

      implicit val config =
        PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)

      object `generated Tiny graph` {
        implicit val arbitraryTinyGraph =
          GraphGen.tinyConnectedIntDi[Graph](Graph)

        def `should conform to tiny metrics` {
          forAll(arbitrary[IntDiGraph]) { g: IntDiGraph =>
            g.order should equal(GraphGen.TinyInt.order)
          }
        }
      }
    }
  }
  RG
  GG
}
