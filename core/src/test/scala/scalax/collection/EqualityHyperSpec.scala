package scalax.collection

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class EqualityHyperSpec extends RefSpec with Matchers {

  def `hyperedges, bag like ` : Unit = {
    import scalax.collection.hyperedges._

    val nodes = List('A', 'B', 'C', 'C')
    val hEdge = HyperEdge.fromUnsafe(nodes)
    hEdge shouldEqual 'A' ~~ 'B' ~~ 'C' ~~ 'C'
    hEdge shouldEqual 'C' ~~ 'C' ~~ 'B' ~~ 'A'

    hEdge.arity shouldBe nodes.size
    for (i <- nodes.indices)
      hEdge.node(i) shouldBe nodes(i)
  }

  def `hyperedges, ordered ` : Unit = {
    import scalax.collection.hyperedges.ordered._

    val nodes = List('A', 'B', 'C', 'C')
    val hEdge = HyperEdge.fromUnsafe(nodes)
    hEdge shouldEqual 'A' ~~ 'B' ~~ 'C' ~~ 'C'
    hEdge shouldNot equal('C' ~~ 'C' ~~ 'B' ~~ 'A')

    hEdge.arity shouldBe nodes.size
    for (i <- nodes.indices)
      hEdge.node(i) shouldBe nodes(i)
  }

  def `directed hyperedges, bag like`: Unit = {
    import scalax.collection.hyperedges._

    val sources = List('A', 'B', 'C')
    val targets = List('D', 'D', 'E')
    val dhEdge  = DiHyperEdge.unsafeFrom(sources, targets)
    dhEdge shouldEqual sources ~~> targets
    dhEdge shouldEqual sources.reverse ~~> targets.reverse

    dhEdge.ends should contain theSameElementsAs (sources ++ targets)

    val sourcesSize = sources.size
    dhEdge.arity shouldBe sourcesSize + targets.size

    for (i <- sources.indices) dhEdge.node(i) shouldBe sources(i)
    for (i <- targets.indices) dhEdge.node(i + sourcesSize) shouldBe targets(i)
  }

  def `directed hyperedges, ordered`: Unit = {
    import scalax.collection.hyperedges.ordered._

    val sources = List('A', 'B', 'C')
    val targets = List('D', 'D', 'E')
    val dhEdge  = DiHyperEdge.unsafeFrom(sources, targets)
    dhEdge shouldEqual sources ~~> targets
    dhEdge shouldNot equal(sources.reverse ~~> targets.reverse)

    dhEdge.ends should contain theSameElementsAs (sources ++ targets)

    val sourcesSize = sources.size
    dhEdge.arity shouldBe sourcesSize + targets.size

    for (i <- sources.indices) dhEdge.node(i) shouldBe sources(i)
    for (i <- targets.indices) dhEdge.node(i + sourcesSize) shouldBe targets(i)
  }
}
