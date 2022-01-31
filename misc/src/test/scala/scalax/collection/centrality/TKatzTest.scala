package scalax.collection.centrality

import scalax.collection.GraphPredef._
import scalax.collection.Graph

import org.scalatest.matchers.should
import org.scalatest.refspec.RefSpec

class TKatzTest extends RefSpec with should.Matchers {

  object `Katz centralities naive implementation works fine with an example at Wikipedia` {
    val (kim, pria, sri, jose, diego, agneta, aziz, bob, john, jane, samantha) =
      ("Kim", "Pria", "Sri", "Jose", "Diego", "Agneta", "Aziz", "Bob", "John", "Jane", "Samantha")

    val network = Graph.from(
      edges = List(
        kim ~ pria,
        kim ~ sri,
        pria ~ sri,
        pria ~ jose,
        sri ~ jose,
        jose ~ diego,
        diego ~ agneta,
        agneta ~ aziz,
        aziz ~ jose,
        jose ~ bob,
        bob ~ john,
        john ~ jane,
        jane ~ aziz,
        jane ~ samantha))

    val katz = new Katz(network)
    val centralities = katz .centralities()

    def `it yields a non-empty map` {
       centralities should be('nonEmpty)
    }

    def `it finds the maximum` {
      centralities.max(katz.mapOrdering)._1 should be(network get jose)
    }
  }
}
