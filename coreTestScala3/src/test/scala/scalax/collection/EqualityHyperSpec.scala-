package scalax.collection

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.generic.AbstractDiHyperEdge

class EqualityHyperSpec extends RefSpec with Matchers {

  def `hyperedges, bag like ` : Unit = {
    import scalax.collection.hyperedges.*

    val nodes = List('A', 'B', 'C', 'C')
    val hEdge = HyperEdge.fromUnsafe(nodes)
    hEdge shouldEqual 'A' ~~ 'B' ~~ 'C' ~~ 'C'
    hEdge shouldEqual 'C' ~~ 'C' ~~ 'B' ~~ 'A'

    hEdge.arity shouldBe nodes.size
    for (i <- nodes.indices)
      hEdge.node(i) shouldBe nodes(i)
  }

  def `hyperedges, ordered ` : Unit = {
    import scalax.collection.hyperedges.ordered.*

    val nodes = List('A', 'B', 'C', 'C')
    val hEdge = HyperEdge.fromUnsafe(nodes)
    hEdge shouldEqual 'A' ~~ 'B' ~~ 'C' ~~ 'C'
    hEdge shouldNot equal('C' ~~ 'C' ~~ 'B' ~~ 'A')

    hEdge.arity shouldBe nodes.size
    for (i <- nodes.indices)
      hEdge.node(i) shouldBe nodes(i)
  }

  def `directed hyperedges, bag like`: Unit = {
    import scalax.collection.hyperedges.*

    val sources = OneOrMore('A', 'B', 'C')
    val targets = OneOrMore('D', 'D', 'E')
    val dhEdge  = DiHyperEdge(sources, targets)
    dhEdge shouldEqual sources ~~> targets
    dhEdge shouldEqual sources.reverse ~~> targets.reverse

    dhEdge.ends.toList should contain theSameElementsAs (sources ++ targets).toList

    val sourcesSize = sources.size
    dhEdge.arity shouldBe sourcesSize + targets.size

    checkIndices(sources, dhEdge)
    checkIndices(targets, dhEdge, sourcesSize)
  }

  def `directed hyperedges, ordered`: Unit = {
    import scalax.collection.hyperedges.ordered.*

    val sources = OneOrMore('A', 'B', 'C')
    val targets = OneOrMore('D', 'D', 'E')
    val dhEdge  = DiHyperEdge(sources, targets)
    dhEdge shouldEqual sources ~~> targets
    dhEdge shouldNot equal(sources.reverse ~~> targets.reverse)

    dhEdge.ends.toList should contain theSameElementsAs (sources ++ targets).toList

    val sourcesSize = sources.size
    dhEdge.arity shouldBe sourcesSize + targets.size

    checkIndices(sources, dhEdge)
    checkIndices(targets, dhEdge, sourcesSize)
  }

  private def checkIndices(s: OneOrMore[_], dhEdge: AbstractDiHyperEdge[_], plus: Int = 0): Unit = {
    val list = s.iterator.toList
    for (i <- list.indices) dhEdge.node(i + plus) shouldBe list(i)
  }
}
