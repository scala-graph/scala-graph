package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.hyperedges._
import scalax.collection.generic.{Edge, GraphCoreCompanion}

class EqualityHyperSpec
    extends Suites(
      new EqualityHyper[immutable.Graph](immutable.Graph),
      new EqualityHyper[mutable.Graph](mutable.Graph)
    )

private class EqualityHyper[CC[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  def `hyperedges ` : Unit = {
    val heNodes = List("A", "B", "C")
    val he      = HyperEdge(heNodes)

    he shouldBe a[HyperEdge[_]]
    he.arity shouldEqual heNodes.size
    he._1 shouldEqual heNodes(0)
    he._2 shouldEqual heNodes(1)
    for (i <- heNodes.indices) he._n(i) should be(heNodes(i))
  }

  def `directed hyperedges ` : Unit = {
    import scalax.collection.hyperedges.DiHyperEdgeImplicits._
    val sources = List('A', 'B', 'C')
    val target  = 'D'
    val dhe     = sources ~~> target

    dhe shouldBe a[DiHyperEdge[_]]
    dhe.ends should contain theSameElementsAs (target +: sources)
    dhe.arity should be(4)
    dhe.sources.head should be(sources.head)
    dhe.sources.last should be(sources.last)
    dhe.targets.head should be(target)
  }
}
