package scalax.collection

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OneOrMore.more
import scalax.collection.Data.shuffleNotEqual
import scalax.collection.generic.{Edge, GenericGraphCoreFactory}

/** Editing hypergraphs with labeled edges including support for multi-hypergraphs.
  */
class EditingLabeledHyperSpec
    extends Suites(
      new LabeledHyperEdges,
      new EditingLabeledHyperEdges[immutable.Graph](immutable.Graph),
      new EditingLabeledHyperEdges[mutable.Graph](mutable.Graph)
    )

object EditingLabeledHyperSpec {
  object Labeled {
    import hyperedges.labeled._

    case class MyHyperEdge[+N](override val ends: Several[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Several[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Several('a', 'b', 'c')
    val label = 1
    val hE    = MyHyperEdge(ends, label)
  }

  object OrderedLabeled {
    import hyperedges.ordered.labeled._

    case class MyHyperEdge[+N](override val ends: Several[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Several[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Labeled.ends
    val label = Labeled.label
    val hE    = MyHyperEdge(ends, label)
  }

  object LabeledDi {
    import hyperedges.labeled._

    case class MyDiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N], label: Int)
        extends LDiHyperEdge[N, Int](sources, targets)
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = more('a', 'b')
    val targets = more('c', 'd')
    val label   = 1
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }

  object OrderedLabeledDi {
    import hyperedges.ordered.labeled._

    case class MyDiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N], label: Int)
        extends LDiHyperEdge[N, Int](sources, targets)
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = more('a', 'b', 'x')
    val targets = more('c', 'd')
    val label   = 1
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }

  object MultiLabeled {
    import hyperedges.multilabeled._

    case class MyHyperEdge[+N](override val ends: Several[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Several[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Labeled.ends
    val label = Labeled.label
    val hE    = MyHyperEdge(ends, label)
  }

  object MultiOrderedLabeled {
    import hyperedges.ordered.multilabeled._

    case class MyHyperEdge[+N](override val ends: Several[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Several[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Labeled.ends
    val label = Labeled.label
    val hE    = MyHyperEdge(ends, label)
  }

  object MultiLabeledDi {
    import hyperedges.multilabeled._

    case class MyDiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N], label: Int)
        extends LDiHyperEdge[N, Int](sources, targets)
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = LabeledDi.sources
    val targets = LabeledDi.targets
    val label   = LabeledDi.label
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }

  object MultiOrderedLabeledDi {
    import hyperedges.ordered.multilabeled._

    case class MyDiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N], label: Int)
        extends LDiHyperEdge[N, Int](sources, targets)
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = OrderedLabeledDi.sources
    val targets = OrderedLabeledDi.targets
    val label   = OrderedLabeledDi.label
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }

  def endsToString(ends: OneOrMore[_]) = ends.iterator.mkString("{", ", ", "}")
}

private class LabeledHyperEdges extends RefSpec with Matchers {
  import EditingLabeledHyperSpec.endsToString

  object `a generic undirected, mappable labeled hyperedge` {
    import hyperedges._
    import hyperedges.labeled._
    import EditingLabeledHyperSpec.Labeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends.iterator mkString " ~~ "} :+ 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldEqual hE
      hE shouldEqual HyperEdge(hE.ends)
      hE shouldEqual HyperEdge(shuffleNotEqual(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' :+ 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      val +: = MyHyperEdge

      hE match {
        case ends +: label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Several.Seq(n1, n2, n3) +: label =>
          val reconstructed = MyHyperEdge(Several(n1, n2, n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
        case _ +: _ => fail()
      }
    }
  }

  object `generic undirected, mappable, ordered labeled hyperedge` {
    import hyperedges._
    import hyperedges.ordered.labeled._
    import EditingLabeledHyperSpec.OrderedLabeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends.iterator mkString " ~~ "} :+ 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldEqual hE
      hE.copy(ends = shuffleNotEqual(hE.ends)) shouldNot equal(hE)
      hE shouldNot equal(HyperEdge(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' :+ 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      val +: = MyHyperEdge

      hE match {
        case ends +: label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Several.Seq(n1, n2, n3) +: label =>
          val reconstructed = MyHyperEdge(Several(n1, n2, n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
        case _ +: _ => fail()
      }
    }
  }

  object `generic directed, mappable labeled hyperedge` {
    import hyperedges._
    import hyperedges.labeled._
    import EditingLabeledHyperSpec.LabeledDi._

    def `meets toString convention`: Unit =
      diHE.toString shouldBe s"${endsToString(sources)} ~~> ${endsToString(targets)} :+ $label"

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldEqual diHE
      diHE shouldEqual DiHyperEdge(diHE.sources, diHE.targets)
      diHE shouldEqual DiHyperEdge(shuffleNotEqual(diHE.sources), shuffleNotEqual(diHE.targets))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets :+ 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object +:   extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets +: label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case OneOrMore.Seq(s1, s2, _*) :~~> OneOrMore.Seq(t1, t2, _*) +: label =>
          val reconstructed = MyDiHyperEdge(
            more(s1, s2),
            more(t1, t2),
            label
          )
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
        case _ :~~> _ +: _ => fail()
      }
    }
  }

  object `generic directed, mappable, ordered labeled hyperedge` {
    import hyperedges._
    import hyperedges.ordered.labeled._
    import EditingLabeledHyperSpec.OrderedLabeledDi._

    def `meets toString convention`: Unit =
      diHE.toString shouldBe s"${endsToString(sources)} ~~> ${endsToString(targets)} :+ $label"

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldEqual diHE
      diHE shouldNot equal(DiHyperEdge(diHE.sources, diHE.targets))
      diHE shouldNot equal(DiHyperEdge(shuffleNotEqual(diHE.sources), shuffleNotEqual(diHE.targets)))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets :+ 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object +:   extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets +: label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case OneOrMore.Seq(s1, s2, sRest @ _*) :~~> OneOrMore.Seq(t1, t2, tRest @ _*) +: label =>
          val reconstructed = MyDiHyperEdge(
            more(s1, s2, sRest: _*),
            more(t1, t2, tRest: _*),
            label
          )
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
        case _ :~~> _ +: _ => fail()
      }
    }
  }

  object `a generic undirected, mappable multilabeled hyperedge` {
    import hyperedges._
    import hyperedges.multilabeled._
    import EditingLabeledHyperSpec.MultiLabeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends.iterator mkString " ~~ "} :++ 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldNot equal(hE)
      hE shouldNot equal(HyperEdge(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' :++ 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      val ++ = MyHyperEdge

      hE match {
        case ends ++ label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Several.Seq(n1, n2, n3) ++ label =>
          val reconstructed = MyHyperEdge(Several(n1, n2, n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
        case _ ++ _ => fail()
      }
    }
  }

  object `generic undirected, mappable, ordered multilabeled hyperedge` {
    import hyperedges._
    import hyperedges.ordered.multilabeled._
    import EditingLabeledHyperSpec.MultiOrderedLabeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends.iterator mkString " ~~ "} :++ 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldNot equal(hE)
      hE.copy(ends = shuffleNotEqual(hE.ends)) shouldNot equal(hE)
      hE shouldNot equal(HyperEdge(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' :++ 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      val ++ = MyHyperEdge

      hE match {
        case ends ++ label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Several.Seq(n1, n2, n3) ++ label =>
          val reconstructed = MyHyperEdge(Several(n1, n2, n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
        case _ ++ _ => fail()
      }
    }
  }

  object `generic directed, mappable multilabeled hyperedge` {
    import hyperedges._
    import hyperedges.multilabeled._
    import EditingLabeledHyperSpec.MultiLabeledDi._

    def `meets toString convention`: Unit =
      diHE.toString shouldBe s"${endsToString(sources)} ~~> ${endsToString(targets)} :++ $label"

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldNot equal(diHE)
      diHE shouldNot equal(DiHyperEdge(diHE.sources, diHE.targets))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets :++ 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object ++:  extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets ++: label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case OneOrMore.Seq(s1, s2, _*) :~~> OneOrMore.Seq(t1, t2, _*) ++: label =>
          val reconstructed = MyDiHyperEdge(
            more(s1, s2),
            more(t1, t2),
            label
          )
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
        case _ :~~> _ ++: _ => fail()
      }
    }
  }

  object `generic directed, mappable, ordered multilabeled hyperedge` {
    import hyperedges._
    import hyperedges.ordered.multilabeled._
    import EditingLabeledHyperSpec.MultiOrderedLabeledDi._

    def `meets toString convention`: Unit =
      diHE.toString shouldBe s"${endsToString(sources)} ~~> ${endsToString(targets)} :++ $label"

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldNot equal(diHE)
      diHE.copy(shuffleNotEqual(diHE.sources), shuffleNotEqual(diHE.targets)) shouldNot equal(diHE)
      diHE shouldNot equal(DiHyperEdge(diHE.sources, diHE.targets))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets :++ 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object ++:  extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets ++: label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case OneOrMore.Seq(s1, s2, sRest @ _*) :~~> OneOrMore.Seq(t1, t2, tRest @ _*) ++: label =>
          val reconstructed = MyDiHyperEdge(
            more(s1, s2, sRest: _*),
            more(t1, t2, tRest: _*),
            label
          )
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
        case _ :~~> _ ++: _ => fail()
      }
    }
  }
}

private class EditingLabeledHyperEdges[G[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, G]](
    val factory: GenericGraphCoreFactory[G]
) extends RefSpec
    with Matchers {

  object `a generic undirected, mappable labeled edge` {
    import EditingLabeledHyperSpec.Labeled._

    def `is mappable`: Unit = {
      val g = factory.from(hE :: Nil)
      g.map(_.toString).edges.toOuter.head shouldBe hE.copy(ends = hE.ends.map(_.toString))
    }
  }

  object `generic undirected, mappable, ordered labeled edge` {
    import EditingLabeledHyperSpec.OrderedLabeled._

    def `is mappable`: Unit = {
      val g = factory.from(hE :: Nil)
      g.map(_.toString).edges.toOuter.head shouldBe hE.copy(ends = hE.ends.map(_.toString))
    }
  }

  object `generic directed, mappable labeled edge` {
    import EditingLabeledHyperSpec.LabeledDi._

    def `is mappable`: Unit = {
      val g = factory.from(diHE :: Nil)
      g.map(_.toString).edges.toOuter.head shouldBe diHE
        .copy(diHE.sources.map(_.toString), diHE.targets.map(_.toString))
    }
  }

  object `generic directed, mappable, ordered labeled edge` {
    import EditingLabeledHyperSpec.OrderedLabeledDi._

    def `is mappable`: Unit = {
      val g = factory.from(diHE :: Nil)
      g.map(_.toString).edges.toOuter.head shouldBe diHE
        .copy(diHE.sources.map(_.toString), diHE.targets.map(_.toString))
    }
  }
}

/* TODO
private class EditingLabeledHyperMutable extends RefSpec with Matchers {
  object `mutable graphs with labeled edges` {
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
  }
}
 */
