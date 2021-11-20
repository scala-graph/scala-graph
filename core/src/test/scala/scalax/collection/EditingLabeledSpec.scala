package scalax.collection

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.GraphEdge.EdgeLike

/** Editing non-hypergraphs with labeled edges, in particular, editing multigraphs.
  */
class EditingLabeledSpec // TODO extends Suites()

private class EditingLabeled[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: ConfigWrapper[CC]
) extends RefSpec
    with Matchers {
  /* TODO
  def `isMulti ` {
    import edge.WkDiEdge
    def multi(g: CC[Int, UnDiEdge], expected: Boolean): Unit = g.isMulti should be(expected)
    val (wDi_1, wDi_2)                                       = (WkDiEdge(1, 2)(0), WkDiEdge(1, 2)(1))

    multi(factory(1 ~ 2), false)
    multi(factory(1 ~ 2, 1 ~> 2), false)
    multi(factory(wDi_1, wDi_2), true)
  }

  def `isDirected ` {
    def directed(g: CC[Int, UnDiEdge], expected: Boolean): Unit = g.isDirected should be(expected)
    val wDi                                                     = edge.WDiEdge(1, 2)(0)

    factory(wDi).isDirected should be(true)
    directed(factory(wDi), true)
    directed(factory(0 ~> 1, wDi), true)
  }
   */
}

private class EditingLabeledMutable extends RefSpec with Matchers {
  object `mutable graphs with labeled edges` { // TODO
    /*
    def `satisfy labeled edege equality` {
      import edge.Implicits._
      import edge.LDiEdge

      type StringLabel = Option[String]
      val str = "A"
      val label: StringLabel = Some(str)
      val g = mutable.Graph(2 ~ 3, (2 ~+# 3) (label))
      g should have('order(2), 'graphSize(2))

      import edge.LBase.{LEdgeImplicits}
      object StringLabelImplicit extends LEdgeImplicits[StringLabel]
      import StringLabelImplicit._
      for (e <- g.edges if e.isLabeled) {
        e.isDefined should be(true)
        e.get should be(str)
      }

      type ListLabel = List[Int]
      implicit val factory = LDiEdge
      val listLabel = List(1, 0, 1)
      g.addLEdge(3, 4)(listLabel) should be(true)
      g should have('order(3), 'graphSize(3))
      val findAdded = g.edges find (3 ~> 4)
      findAdded should be('isDefined)
      val added: g.EdgeT = findAdded.get
      added.directed should be(true)
      added.count(_ > 0) should be(List(1, 0, 1).count(_ > 0))
    }

    def `are upsertable` {
      import edge.LDiEdge
      val (label, modLabel) = ("A", "B")
      val g                 = mutable.Graph(LDiEdge(1, 2)(label), LDiEdge(2, 3)(label))

      g.edges foreach {
        _.edge match {
          case LDiEdge(s, t, l) => g upsert (LDiEdge(s.value, t.value)(modLabel))
        }
      }
      g should have('graphSize (2))
      g.edges foreach { _.label should be(modLabel) }
    }
     */
  }
}
