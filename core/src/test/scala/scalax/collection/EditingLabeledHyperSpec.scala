package scalax.collection

import scala.collection.immutable.Iterable
import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.Data.shuffleNotEqual
import scalax.collection.generic.{Edge, Ends, GraphCoreCompanion}

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

    case class MyHyperEdge[+N](override val ends: Ends[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Ends[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Ends('a', 'b', 'c')
    val label = 1
    val hE    = MyHyperEdge(ends, label)
  }

  object OrderedLabeled {
    import hyperedges.ordered.labeled._

    case class MyHyperEdge[+N](override val ends: Ends[N], label: Int)
        extends LHyperEdge[N, Int](ends)
        with GenericHyperEdgeMapper[MyHyperEdge] {
      def map[N](ends: Ends[N]): MyHyperEdge[N] = copy(ends)
    }

    val ends  = Labeled.ends
    val label = Labeled.label
    val hE    = MyHyperEdge(ends, label)
  }

  object LabeledDi {
    import hyperedges.labeled._

    case class MyDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N], label: Int)
        extends LDiHyperEdge[N, Int]
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: Iterable[N], targets: Iterable[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = List('a', 'b')
    val targets = List('c', 'd')
    val label   = 1
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }

  object OrderedLabeledDi {
    import hyperedges.ordered.labeled._

    case class MyDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N], label: Int)
        extends LDiHyperEdge[N, Int]
        with GenericDiHyperEdgeMapper[MyDiHyperEdge] {
      def map[N](sources: Iterable[N], targets: Iterable[N]): MyDiHyperEdge[N] = copy(sources, targets)
    }

    val sources = List('a', 'b', 'x')
    val targets = List('c', 'd')
    val label   = 1
    val diHE    = MyDiHyperEdge(sources, targets, label)
  }
}

private class LabeledHyperEdges extends RefSpec with Matchers {

  object `a generic undirected, mappable labeled edge` {
    import hyperedges._
    import hyperedges.labeled._
    import EditingLabeledHyperSpec.Labeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends mkString " ~~ "} + 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldEqual hE
      hE shouldEqual HyperEdge.fromUnsafe(hE.ends)
      hE shouldEqual HyperEdge.fromUnsafe(shuffleNotEqual(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' + 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      import generic.UnapplyGenericLabeledHyperEdge
      object + extends UnapplyGenericLabeledHyperEdge[MyHyperEdge, Int]

      hE match {
        case ends + label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Ends(n1 :: n2 :: n3 :: _) + label =>
          val reconstructed = MyHyperEdge(Ends.fromUnsafe(n1 :: n2 :: n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
    }
  }

  object `generic undirected, mappable, ordered labeled edge` {
    import hyperedges._
    import hyperedges.ordered.labeled._
    import EditingLabeledHyperSpec.OrderedLabeled._

    def `meets toString convention`: Unit =
      hE.toString shouldBe s"${ends mkString " ~~ "} + 1"

    def `meets equality rules`: Unit = {
      hE.copy(label = 9) shouldEqual hE
      hE.copy(ends = shuffleNotEqual(hE.ends)) shouldNot equal(hE)
      hE shouldNot equal(HyperEdge.fromUnsafe(hE.ends))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val hyperedge: HyperEdge[N])
          extends LHyperEdgeInfixConstructor[N, Int, MyHyperEdge](MyHyperEdge.apply)
      'a' ~~ 'b' ~~ 'c' + 1 shouldEqual hE
    }

    def `supports infix extraction`: Unit = {
      import generic.UnapplyGenericLabeledHyperEdge
      object + extends UnapplyGenericLabeledHyperEdge[MyHyperEdge, Int]

      hE match {
        case ends + label =>
          val reconstructed = MyHyperEdge(ends, label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
      hE match {
        case Ends(n1 :: n2 :: n3 :: _) + label =>
          val reconstructed = MyHyperEdge(Ends.fromUnsafe(n1 :: n2 :: n3 :: Nil), label)
          "reconstructed: MyHyperEdge[Char]" should compile
          reconstructed shouldEqual hE
      }
    }
  }

  object `generic directed, mappable labeled edge` {
    import hyperedges._
    import hyperedges.labeled._
    import EditingLabeledHyperSpec.LabeledDi._

    def `meets toString convention`: Unit = {
      def nodesToString(nodes: Iterable[_]) = nodes.mkString("{", ", ", "}")
      diHE.toString shouldBe s"${nodesToString(sources)} ~~> ${nodesToString(targets)} + $label"
    }

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldEqual diHE
      diHE shouldEqual DiHyperEdge.unsafeFrom(diHE.sources, diHE.targets)
      diHE shouldEqual DiHyperEdge.unsafeFrom(shuffleNotEqual(diHE.sources), shuffleNotEqual(diHE.targets))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets + 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object +    extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets + label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case (s1 :: s2 :: _) :~~> (t1 :: t2 :: _) + label =>
          val reconstructed = MyDiHyperEdge(List(s1, s2), List(t1, t2), label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
    }
  }

  object `generic directed, mappable, ordered labeled edge` {
    import hyperedges._
    import hyperedges.ordered.labeled._
    import EditingLabeledHyperSpec.OrderedLabeledDi._

    def `meets toString convention`: Unit = {
      def nodesToString(nodes: Iterable[_]) = nodes.mkString("{", ", ", "}")
      diHE.toString shouldBe s"${nodesToString(sources)} ~~> ${nodesToString(targets)} + $label"
    }

    def `meets equality rules`: Unit = {
      diHE.copy(label = 9) shouldEqual diHE
      diHE shouldNot equal(DiHyperEdge.unsafeFrom(diHE.sources, diHE.targets))
      diHE shouldNot equal(DiHyperEdge.unsafeFrom(shuffleNotEqual(diHE.sources), shuffleNotEqual(diHE.targets)))
    }

    def `supports infix construction`: Unit = {
      implicit class MyInfixConstructor[N](val diHyperedge: DiHyperEdge[N])
          extends LDiHyperEdgeInfixConstructor[N, Int, MyDiHyperEdge](MyDiHyperEdge.apply)
      sources ~~> targets + 1 shouldEqual diHE
    }

    def `supports infix extraction`: Unit = {
      import generic.{UnapplyGenericHyperLabel, UnapplyGenericLabeledDiHyperEdge}
      object :~~> extends UnapplyGenericLabeledDiHyperEdge[MyDiHyperEdge, Int]
      object +    extends UnapplyGenericHyperLabel[Int]

      diHE match {
        case sources :~~> targets + label =>
          val reconstructed = MyDiHyperEdge(sources, targets, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
      diHE match {
        case (s1 :: s2 :: sRest) :~~> (t1 :: t2 :: tRest) + label =>
          val reconstructed = MyDiHyperEdge(List(s1, s2) ++ sRest, List(t1, t2) ++ tRest, label)
          "reconstructed: MyDiHyperEdge[Char]" should compile
          reconstructed shouldEqual diHE
      }
    }
  }
}

private class EditingLabeledHyperEdges[G[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G]
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
