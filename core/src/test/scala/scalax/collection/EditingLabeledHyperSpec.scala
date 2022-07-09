package scalax.collection

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

/** Editing hypergraphs with labeled edges, in particular, editing multi-hypergraphs.
  */
class EditingLabeledHyperSpec
    extends Suites(
      new EditingLabeledHyperEdges
      /* TODO
    new EditingLabeled[immutable.Graph](immutable.Graph),
    new EditingLabeled[mutable.Graph](mutable.Graph) */
    )

private class EditingLabeledHyperEdges extends RefSpec with Matchers {

  /*
  def `toString of labeled hyper edge`: Unit =
    x.toString should startWith(s"$madrid ~> $rio + ")
   */
}

private class EditingLabeledHyperMutable extends RefSpec with Matchers {
  object `mutable graphs with labeled edges` { // TODO
    /*
        def `satisfy labeled directed hyperedege equality` {
          import edge.Implicits._
          import edge.LHyperEdge

          type StringLabel = String
          val outerLabels = Seq("A", "BC", "CDE")
          val g = mutable.Graph(1 ~ 2 ~ 3, (2 ~+# 3) (outerLabels(0)))

          implicit val factory = LHyperEdge
          (g +~+= (3, 4, 5)) (outerLabels(1))
          g should have('order(5), 'size(3))
          g.addLEdge(4, 5, 6)(outerLabels(2)) should be(true)
          g should have('order(6), 'size(4))

          val innerLabels: collection.mutable.Set[_ >: StringLabel] =
            g.edges filter (_.isLabeled) map (_.label)
          innerLabels should have size (outerLabels.size)
          /*
        innerLabels forall (outerLabels contains _) should be (true)
     * https://groups.google.com/forum/?fromgroups=#!searchin/scala-internals/both$20method/scala-internals/nPZY2EMtDvY/PivCCtyRM_IJ
     * https://issues.scala-lang.org/browse/SI-5330
     */
          (innerLabels: Iterable[Any]) forall (outerLabels contains _) should be(true)
        }
     */
  }
}
