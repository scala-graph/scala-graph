package scalax.collection

import org.scalatest.Spec
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._, edge._, edge.LBase._, edge.Implicits._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import custom.flight._,
       custom.flight.Helper._,
       custom.flight.FlightImplicits._

@RunWith(classOf[JUnitRunner])
class TEdgeTest extends Spec with ShouldMatchers
{
  object FlightLabel extends LEdgeImplicits[Flight]
  import FlightLabel._

  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo = "LH007"

  def test_LkDiEdge {
    val outer = LkDiEdge(ham, gig)(Flight(flightNo))
    val g = Graph(outer)
    val e  = g.edges.head
    e.edge.nodes.productElement(0).asInstanceOf[AnyRef].getClass should be (
        g.nodes.head.getClass)
    e.from     should be (ham)
    e.to       should be (gig)
    e.flightNo should be (flightNo)
    e          should be (outer)
    e.##       should be (outer.##)
    val eqFlight = LkDiEdge(ham, gig)(Flight(flightNo, 11 o 2))
    e          should be (eqFlight)
    e.##       should be (eqFlight.##)
    val neFlight = LkDiEdge(ham, gig)(Flight(flightNo + "x", 11 o 2))
    e          should not be (neFlight)
    e.##       should not be (neFlight.##)
  }
  def test_LkDiEdgeShortcut {
    val outer = LkDiEdge(ham, gig)(Flight(flightNo))
    (ham ~+#> gig)(Flight(flightNo))          should be (outer)
    (ham ~+#> gig)(Flight(flightNo, 11 o 20)) should be (outer)
  }
  def test_match_W {
    val (n1, n2, w) = (1, 2, 5)
    def check(_n1: Int, _n2: Int, _w: Long) {
      _n1 should be (n1)
      _n2 should be (n2)
      _w  should be (w)
    }
    val wDi = (n1 ~%> n2)(w)
    wDi match { case WDiEdge(s, t, w) => check(s, t, w) }
    wDi match { case s :~> %(t, w)    => check(s, t, w) }
    wDi match { case s :~> t % w      => check(s, t, w) }
    Graph(wDi).get(wDi).edge match {
      case s :~> t % w => check(s.value, t.value, w) }
    
    val wkDi = (n1 ~%#> n2)(w)
    wkDi match { case s :~> t % w     => check(s, t, w) }
  }
  def test_match_L {
   object StringLabel extends LEdgeImplicits[String]
   import StringLabel._

   val (n1, n2, label) = (1, 2, "A")
    def check(_n1: Int, _n2: Int, _label: String) {
      _n1 should be (n1)
      _n2 should be (n2)
      _label should be (label)
    }
    val lDi = (n1 ~+> n2)(label)
    lDi match { case LDiEdge(s, t, l) => check(s, t, l) }
    lDi match { case s :~> +(t, l)    => check(s, t, l) }
    lDi match { case s :~> t + l      => check(s, t, l) }
    Graph(lDi).get(lDi).edge match {
      case s :~> t + l => check(s.value, t.value, l) }

    val lkDi = (n1 ~+#> n2)(label)
    lkDi match { case s :~> t + l     => check(s, t, l) }
  }
  def test_match_WL {
   object StringLabel extends LEdgeImplicits[String]
   import StringLabel._

   val (n1, n2, label, weight) = (1, 2, "A", 4L)
    def check(_n1: Int, _n2: Int, _weight: Long, _label: String) {
      _n1 should be (n1)
      _n2 should be (n2)
      _weight should be (weight)
      _label should be (label)
    }
    val wlDi = (n1 ~%+> n2)(weight, label)
    wlDi match { case WLDiEdge(s, t, w, l) => check(s, t, w, l) }
    wlDi match { case s :~> %+(t, w, l)    => check(s, t, w, l) }
    wlDi match { case s :~> t %+ (w, l)    => check(s, t, w, l) }
    Graph(wlDi).get(wlDi).edge match {
      case s :~> t %+ (w, l) => check(s.value, t.value, w, l) }

    val wlkDi = (n1 ~%+#> n2)(weight, label)
    wlkDi match { case s :~> t %+ (w, l)   => check(s, t, w, l) }
  }
  def test_findOutgoingToDi {
    import edge.LkDiEdge
    val le = LkDiEdge(1,1)(1)
    val lg = Graph(le)
    val ln1 = lg get 1
    (ln1 findOutgoingTo ln1) should be (Some(le))
  }
}
/* Label type for use in key-labeled edges.
 * Note that using path-dependent label types with Scala 2.9.1-final I had a runtime issue
 * which could be resolved by moving the label class to the top-level.
 */
case class Flight(val flightNo: String,
                  val departure: DayTime = DayTime(0,0),
                  val duration: Duration = Duration(0,0))
{
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
  val h = 2~4~6
  val d = 1~>2
  val u = 1~(-1)

  val (lh1, lh2) = (LHyperEdge(1,3,5)(6), LHyperEdge(1,3,5)(7))
  val g_lh_h  = Graph(lh1,h)
  val g_lh_d  = Graph[Int,HyperEdge](lh1,d) // not inferred
  val g_lh_lh = Graph(lh1,lh2)

  val (lkh1, lkh2) = (LkHyperEdge(1,3,5)(8), LkHyperEdge(1,3,5)(9))
  val g_lkh_h   = Graph(lkh1,h)
  val g_lkh_lkh = Graph(lkh1,lkh2)
  val g_lkh_lh  = Graph(lkh1,lh1)

  val (ldh1, ldh2) = (LDiHyperEdge(1,3,5)(10), LDiHyperEdge(1,3,5)(11))
  val g_ldh_h   = Graph(ldh1,h)
  val g_ldh_ldh = Graph(ldh1,ldh2)
  val g_ldh_lh  = Graph(ldh1,lh2)
  val g_ldh_lkh = Graph[Int,LHyperEdge](ldh1,lkh2) // not inferred

  val (lkdh1, lkdh2) = (LkDiHyperEdge(1,3,5)(12), LkDiHyperEdge(1,3,5)(13))
  val g_lkdh_h    = Graph(lkdh1,h)
  val g_lkdh_lkdh = Graph(lkdh1,lkdh2)
  val g_lkdh_ldh  = Graph(lkdh1,ldh2)
  val g_lkdh_lh   = Graph(lkdh1,lh2)
  val g_lkdh_lkh  = Graph[Int,LHyperEdge](lkdh1,lkh2) // not inferred

  val (lu1, lu2) = (LUnDiEdge(1,3)(4), LUnDiEdge(1,3)(5))
  val g_lu_u  = Graph(lu1,u)
  val g_lu_h  = Graph(lu1,h)
  val g_lu_d  = Graph[Int,UnDiEdge](lu1,d) // not inferred
  val g_lu_lu = Graph(lu1,lu2)
  val g_lu_lh = Graph[Int,HyperEdge](lu1,lh2) // not inferred
}
// Compiler tests for calling label methods by means of implicits.  
object TestImplicits {
  import scalax.collection.Graph
  case class MyLabel(val i: Int)
  val eOuter = LUnDiEdge(1,3)(MyLabel(4))

  object OuterEdge {
    object UserL extends LEdgeImplicits[MyLabel]
    import UserL._
    val four = eOuter.i
  }
  object InnerEdge {
    object UserL extends LEdgeImplicits[MyLabel]
    import UserL._
    val g = Graph(eOuter)
    val eInner = g.edges.head
    
    // val four_0 = e.label match {case m: MyLabel => m.i} 
    val four = eInner.i
  }
}
// Compiler tests for predefined edge shortcuts.  
object TestOperators {
  val ld  = (1 ~+>  2)(3)
  val lkd = (3 ~+#> 4)(7)
}