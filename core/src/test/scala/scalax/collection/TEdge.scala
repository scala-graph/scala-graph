package scalax.collection

import scala.language.higherKinds

import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.refspec.RefSpec
import org.junit.runner.RunWith

import GraphPredef._, GraphEdge._, edge._, edge.LBase._, edge.Implicits._

import custom.flight._, custom.flight.Helper._

@RunWith(classOf[JUnitRunner])
class TEdgeTest extends RefSpec with Matchers {

  object `WDiEdge ` {
    def `can have weight overridden`: Unit = {
      val edge    = "A" ~> "B" % 1
      val newEdge = edge       % 2
      val oldEdge = newEdge    % 1

      edge.weight should ===(1)
      newEdge.weight should ===(2)
      oldEdge should ===(newEdge)
    }
  }

  object `LDiEdge ` {
    def `can have label overridden`: Unit = {
      val edge    = "A" ~> "B" + 1
      val newEdge = edge + 2
      val oldEdge = newEdge + 1

      edge.label should ===(1)
      newEdge.label should ===(2)
      oldEdge should ===(newEdge)
    }
  }

  trait OrderedEndpointsTest[E[X] <: EdgeLike[X]] {

    def ordered(edges: Traversable[_]): Boolean =
      edges forall (_.isInstanceOf[OrderedEndpoints])

    def outerEdges(implicit kind: CollectionKind = Bag): List[E[_]]

    def `are treated as a bag by default` {
      val edges = outerEdges()
      edges(1) should be(edges(2))
      val g = Graph(edges: _*)
      g.graphSize should be(2)
      ordered(g.edges) should be(false)
    }

    def `may be defined to be sorted.` {
      val edges = outerEdges(Sequence)
      edges(1) should not equal (edges(2))
      val g = Graph(edges: _*)
      g.graphSize should be(3)
      ordered(g.edges) should be(true)
      ordered((g - g.edges.head).edges) should be(true)
    }
  }

  object `DiHyperEdge target nodes` extends OrderedEndpointsTest[DiHyperEdge] {
    def outerEdges(implicit kind: CollectionKind = Bag): List[DiHyperEdge[Int]] =
      DiHyperEdge(1, 2, 2) ::
        DiHyperEdge(1, 2, 2, 3) ::
        DiHyperEdge(1, 2, 3, 2) :: Nil
  }

  object `LHyperEdge target nodes` extends OrderedEndpointsTest[LHyperEdge] {
    import edge.LHyperEdge, edge.Implicits._
    def outerEdges(implicit kind: CollectionKind = Bag): List[LHyperEdge[Int]] =
      (1 ~ 2 ~+ 2)('a') ::
        (1 ~ 2 ~ 2 ~+ 3)('b') ::
        (1 ~ 2 ~ 3 ~+ 2)('c') :: Nil
  }

  object FlightLabel extends LEdgeImplicits[Flight]
  import FlightLabel._

  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo   = "LH007"

  object `Custom edge tests` {

    def `LkDiEdge ` {
      val outer = LkDiEdge(ham, gig)(Flight(flightNo))
      val g     = Graph(outer)
      val e     = g.edges.head
      e.edge.nodes.productElement(0).asInstanceOf[AnyRef].getClass should be(g.nodes.head.getClass)
      e.from should be(ham)
      e.to should be(gig)
      e.flightNo should be(flightNo)
      e should be(outer)
      e.## should be(outer.##)
      val eqFlight = LkDiEdge(ham, gig)(Flight(flightNo, 11 o 2))
      e should be(eqFlight)
      e.## should be(eqFlight.##)
      val neFlight = LkDiEdge(ham, gig)(Flight(flightNo + "x", 11 o 2))
      e should not be (neFlight)
      e.## should not be (neFlight.##)
    }

    def `LkDiEdgeShortcut ` {
      val outer = LkDiEdge(ham, gig)(Flight(flightNo))
      (ham ~+#> gig)(Flight(flightNo)) should be(outer)
      (ham ~+#> gig)(Flight(flightNo, 11 o 20)) should be(outer)
    }

    def `matching weighted edges` {
      val (n1, n2, w) = (1, 2, 5)
      def check(_n1: Int, _n2: Int, _w: Double) {
        _n1 should be(n1)
        _n2 should be(n2)
        _w should be(w)
      }
      val wDi = (n1 ~%> n2)(w)
      wDi match { case WDiEdge(s, t, w) => check(s, t, w) }
      wDi match { case s :~> %(t, w)    => check(s, t, w) }
      wDi match { case s :~> t % w      => check(s, t, w) }
      Graph(wDi).get(wDi).edge match {
        case s :~> t % w => check(s.value, t.value, w)
      }

      val wkDi = (n1 ~%#> n2)(w)
      wkDi match { case s :~> t % w => check(s, t, w) }
    }

    def `matching labeled edges` {
      object StringLabel extends LEdgeImplicits[String]
      import StringLabel._

      val (n1, n2, label) = (1, 2, "A")
      def check(_n1: Int, _n2: Int, _label: String) {
        _n1 should be(n1)
        _n2 should be(n2)
        _label should be(label)
      }
      val lDi = (n1 ~+> n2)(label)
      lDi match { case LDiEdge(s, t, l) => check(s, t, l) }
      lDi match { case s :~> +(t, l)    => check(s, t, l) }
      lDi match { case s :~> t + l      => check(s, t, l) }
      Graph(lDi).get(lDi).edge match {
        case s :~> t + l => check(s.value, t.value, l)
      }

      val lkDi = (n1 ~+#> n2)(label)
      lkDi match { case s :~> t + l => check(s, t, l) }
    }

    def `matching weighted labeled edges` {
      object StringLabel extends LEdgeImplicits[String]
      import StringLabel._

      val (n1, n2, label, weight) = (1, 2, "A", 4d)
      def check(_n1: Int, _n2: Int, _weight: Double, _label: String) {
        _n1 should be(n1)
        _n2 should be(n2)
        _weight should be(weight)
        _label should be(label)
      }
      val wlDi = (n1 ~%+> n2)(weight, label)
      wlDi match { case WLDiEdge(s, t, w, l) => check(s, t, w, l) }
      wlDi match { case s :~> %+(t, w, l)    => check(s, t, w, l) }
      wlDi match { case s :~> t %+ (w, l)    => check(s, t, w, l) }
      Graph(wlDi).get(wlDi).edge match {
        case s :~> t %+ (w, l) => check(s.value, t.value, w, l)
      }

      val wlkDi = (n1 ~%+#> n2)(weight, label)
      wlkDi match { case s :~> t %+ (w, l) => check(s, t, w, l) }
    }

    def `findOutgoingTo LkDiEdge` {
      import edge.LkDiEdge
      val le  = LkDiEdge(1, 1)(1)
      val lg  = Graph(le)
      val ln1 = lg get 1
      (ln1 findOutgoingTo ln1) should be(Some(le))
    }

    def `LkHyperEdge equality` {
      val e1 = LkDiHyperEdge(1, 1)("a")
      val e2 = LkHyperEdge(1, 1)("b")
      val g  = Graph[Int, LHyperEdge](e1, e2)

      g find e1 should be('defined)
      g find e2 should be('defined)
    }

    def `LkDiHyperEdge equality` {
      val e  = LkDiHyperEdge(1, 2, 3)("a")
      val g  = Graph[Int, LHyperEdge](e)
      val eo = g.edges.head.toOuter

      g find eo should be('defined)
    }
  }
}

/* Label type for use in key-labeled edges.
 */
case class Flight(flightNo: String, departure: DayTime = DayTime(0, 0), duration: Duration = Duration(0, 0)) {
  /* flightNo should be treated as the label key meaning that the set of edges
   * incident to two given nodes may contain at most one edge with a given flightNo.
   *
   * To achieve the above requirement we must override `equals` and `hashCode`
   * narrowing equality to the flightNo attribute because the hash-code of key-labeled edges
   * is composed by the hash-code of the incident nodes and the label hash-code.
   */
  override def equals(other: Any) = other match {
    case that: Flight => that.flightNo == this.flightNo
    case _            => false
  }
  override def hashCode = flightNo.##
}

// Compiler tests for predefined edges.
object Test {
  import scalax.collection.GraphPredef._
  val h = 2 ~ 4 ~ 6
  val d = 1 ~> 2
  val u = 1 ~ (-1)

  val (lh1, lh2) = (LHyperEdge(1, 3, 5)(6), LHyperEdge(1, 3, 5)(7))
  val g_lh_h     = Graph(lh1, h)
  val g_lh_d     = Graph[Int, HyperEdge](lh1, d) // not inferred
  val g_lh_lh    = Graph(lh1, lh2)

  val (lkh1, lkh2) = (LkHyperEdge(1, 3, 5)(8), LkHyperEdge(1, 3, 5)(9))
  val g_lkh_h      = Graph(lkh1, h)
  val g_lkh_lkh    = Graph(lkh1, lkh2)
  val g_lkh_lh     = Graph(lkh1, lh1)

  val (ldh1, ldh2) = (LDiHyperEdge(1, 3, 5)(10), LDiHyperEdge(1, 3, 5)(11))
  val g_ldh_h      = Graph(ldh1, h)
  val g_ldh_ldh    = Graph(ldh1, ldh2)
  val g_ldh_lh     = Graph(ldh1, lh2)
  val g_ldh_lkh    = Graph[Int, LHyperEdge](ldh1, lkh2) // not inferred

  val (lkdh1, lkdh2) = (LkDiHyperEdge(1, 3, 5)(12), LkDiHyperEdge(1, 3, 5)(13))
  val g_lkdh_h       = Graph(lkdh1, h)
  val g_lkdh_lkdh    = Graph(lkdh1, lkdh2)
  val g_lkdh_ldh     = Graph(lkdh1, ldh2)
  val g_lkdh_lh      = Graph(lkdh1, lh2)
  val g_lkdh_lkh     = Graph[Int, LHyperEdge](lkdh1, lkh2) // not inferred

  val (lu1, lu2) = (LUnDiEdge(1, 3)(4), LUnDiEdge(1, 3)(5))
  val g_lu_u     = Graph(lu1, u)
  val g_lu_h     = Graph(lu1, h)
  val g_lu_d     = Graph[Int, UnDiEdge](lu1, d) // not inferred
  val g_lu_lu    = Graph(lu1, lu2)
  val g_lu_lh    = Graph[Int, HyperEdge](lu1, lh2) // not inferred
}

// Compiler tests for calling label methods by means of implicits.
object TestImplicits {
  import scalax.collection.Graph
  case class MyLabel(val i: Int)
  val eOuter = LUnDiEdge(1, 3)(MyLabel(4))

  object OuterEdge {
    object UserL extends LEdgeImplicits[MyLabel]
    import UserL._
    val four = eOuter.i
  }
  object InnerEdge {
    object UserL extends LEdgeImplicits[MyLabel]
    import UserL._
    val g      = Graph(eOuter)
    val eInner = g.edges.head

    // val four_0 = e.label match {case m: MyLabel => m.i}
    val four = eInner.i
  }
}

// Compiler tests for predefined edge shortcuts.
object TestOperators {
  val ld  = (1 ~+> 2)(3)
  val lkd = (3 ~+#> 4)(7)
}
