package scalax.collection

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.edges.{DiEdge, UnDiEdge}
import scalax.collection.generic.{Edge, GraphCoreCompanion}

/** Editing non-hypergraphs with labeled edges, in particular, editing multigraphs.
  */
class EditingLabeledSpec
    extends Suites(
      new LabeledEdges,
      new EditingLabeledEdges[immutable.Graph](immutable.Graph),
      new EditingLabeledEdges[mutable.Graph](mutable.Graph)
    )

private class LabeledEdges extends RefSpec with Matchers {

  def `toString of labeled edge`: Unit = {
    import edges.labeled._

    WUnDiEdge('a', 'b', 2).toString shouldBe "a ~ b % 2.0"
    WUnDiEdge("A", "B", 3).toString shouldBe "A ~ B % 3.0"
  }

  def `toString of multilabeled edge`: Unit = {
    import edges.multilabeled._

    WUnDiEdge('a', 'b', 2).toString shouldBe "a ~ b %% 2.0"
    WUnDiEdge("A", "B", 3).toString shouldBe "A ~ B %% 3.0"
  }

  def `mixed infix constructors`: Unit = {
    import edges.UnDiEdgeImplicits
    import edges.labeled._
    import edges.multilabeled._

    1 ~ 2  % 3.2 shouldBe a[edges.labeled.WUnDiEdge[_]]
    1 ~ 2 %% 3.2 shouldBe a[edges.multilabeled.WUnDiEdge[_]]
  }

  def `mixed infix extractors`: Unit = {
    import edges.UnDiEdgeImplicits
    import edges.labeled._
    import edges.multilabeled._

    1 ~ 2 % 3.2 match {
      case n1 :~ n2 % w => (n1, n2, w) shouldBe (1, 2, 3.2)
    }
    1 ~ 2 %% 3.2 match {
      case n1 ::~ n2 %% w => (n1, n2, w) shouldBe (1, 2, 3.2)
    }
  }
}

private class EditingLabeledEdges[G[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G]
) extends RefSpec
    with Matchers {

  def `generic directed labeled edge`: Unit = {
    import edges.labeled.LDiEdge

    case class MyEdge[+N](source: N, target: N, label: String) extends LDiEdge[N, String]

    val e = MyEdge(1, 2, "")
    e shouldEqual DiEdge(1, 2)

    val g = factory.from(e :: Nil)
    "g.map(_.toString)" shouldNot compile
  }

  def `generic directed, mappable labeled edge`: Unit = {
    import edges.labeled._

    case class MyEdge[+N](source: N, target: N, label: String)
        extends LDiEdge[N, String]
        with GenericEdgeMapper[MyEdge] {
      def map[N](source: N, target: N) = copy(source, target)
    }

    val e = MyEdge(1, 2, "")
    e shouldEqual DiEdge(1, 2)

    val g = factory.from(e :: Nil)
    g.map(_.toString).edges.toOuter.head shouldBe MyEdge("1", "2", "")

    import edges.DiEdgeImplicits
    implicit class MyInfixConstructor[N](val edge: DiEdge[N])
        extends LDiEdgeInfixConstructor[N, String, MyEdge](MyEdge.apply)
    1 ~> 2 + "" shouldEqual e

    import generic.{UnapplyGenericLabel, UnapplyGenericLabeledEdge}
    object :~> extends UnapplyGenericLabeledEdge[MyEdge, String]
    object +   extends UnapplyGenericLabel[String]

    e match {
      case n1 :~> n2 + label =>
        val reconstructed = MyEdge(n1, n2, label)
        "reconstructed: MyEdge[Int]" should compile
        reconstructed shouldEqual e
    }
  }

  def `generic undirected labeled edge`: Unit = {
    import edges.labeled.LUnDiEdge

    case class MyEdge[+N](source: N, target: N, label: String) extends LUnDiEdge[N, String]

    val e = MyEdge(1, 2, "")
    e shouldEqual UnDiEdge(1, 2)

    val g = factory.from(e :: Nil)
    "g.map(_.toString)" shouldNot compile
  }

  def `generic undirected, mappable labeled edge`: Unit = {
    import edges.labeled._

    case class MyEdge[+N](source: N, target: N, label: String)
        extends LUnDiEdge[N, String]
        with GenericEdgeMapper[MyEdge] {
      def map[N](node_1: N, node_2: N) = copy(node_1, node_2)
    }

    val e = MyEdge(1, 2, "")
    e shouldEqual UnDiEdge(1, 2)

    val g = factory.from(e :: Nil)
    g.map(_.toString).edges.toOuter.head shouldBe MyEdge("1", "2", "")

    import edges.UnDiEdgeImplicits
    implicit class MyInfixConstructor[N](val edge: UnDiEdge[N])
        extends LUnDiEdgeInfixConstructor[N, String, MyEdge](MyEdge.apply)
    1 ~ 2 + "" shouldEqual e

    import generic.{UnapplyGenericLabel, UnapplyGenericLabeledEdge}
    object :~ extends UnapplyGenericLabeledEdge[MyEdge, String]
    object +  extends UnapplyGenericLabel[String]

    e match {
      case n1 :~ n2 + label =>
        val reconstructed = MyEdge(n1, n2, label)
        "reconstructed: MyEdge[Int]" should compile
        reconstructed shouldEqual e
    }
  }

  def `generic directed labeled multiedge`: Unit = {
    import edges.multilabeled.LDiEdge

    case class MyEdge[+N](source: N, target: N, label: String) extends LDiEdge[N, String]

    val e = MyEdge(1, 2, "")
    e shouldNot equal(DiEdge(1, 2))

    val g = factory.from(e :: Nil)
    "g.map(_.toString)" shouldNot compile
  }

  def `generic directed, mappable labeled multiedge`: Unit = {
    import edges.multilabeled._

    case class MyEdge[+N](source: N, target: N, label: String)
        extends LDiEdge[N, String]
        with GenericEdgeMapper[MyEdge] {
      def map[N](source: N, target: N) = copy(source, target)
    }

    val e = MyEdge(1, 2, "")
    e shouldNot equal(DiEdge(1, 2))

    val g = factory.from(e :: Nil)
    g.map(_.toString).edges.toOuter.head shouldBe MyEdge("1", "2", "")

    import edges.DiEdgeImplicits
    implicit class MyInfixConstructor[N](val edge: DiEdge[N])
        extends LDiEdgeInfixConstructor[N, String, MyEdge](MyEdge.apply)
    1 ~> 2 ++ "" shouldEqual e

    import generic.{UnapplyGenericLabel, UnapplyGenericLabeledEdge}
    object :~> extends UnapplyGenericLabeledEdge[MyEdge, String]
    object ++  extends UnapplyGenericLabel[String]

    e match {
      case n1 :~> n2 ++ label =>
        val reconstructed = MyEdge(n1, n2, label)
        "reconstructed: MyEdge[Int]" should compile
        reconstructed shouldEqual e
    }
  }

  def `generic undirected labeled multiedge`: Unit = {
    import edges.multilabeled.LUnDiEdge

    case class MyEdge[+N](source: N, target: N, label: String) extends LUnDiEdge[N, String]

    val e = MyEdge(1, 2, "")
    e shouldNot equal(UnDiEdge(1, 2))

    val g = factory.from(e :: Nil)
    "g.map(_.toString)" shouldNot compile
  }

  def `generic undirected, mappable labeled multiedge`: Unit = {
    import edges.multilabeled._

    case class MyEdge[+N](source: N, target: N, label: String)
        extends LUnDiEdge[N, String]
        with GenericEdgeMapper[MyEdge] {
      def map[N](node_1: N, node_2: N) = copy(node_1, node_2)
    }

    val e = MyEdge(1, 2, "")
    e shouldNot equal(UnDiEdge(1, 2))

    val g = factory.from(e :: Nil)
    g.map(_.toString).edges.toOuter.head shouldBe MyEdge("1", "2", "")

    import edges.UnDiEdgeImplicits
    implicit class MyInfixConstructor[N](val edge: UnDiEdge[N])
        extends LUnDiEdgeInfixConstructor[N, String, MyEdge](MyEdge.apply)
    1 ~ 2 ++ "" shouldEqual e

    import generic.{UnapplyGenericLabel, UnapplyGenericLabeledEdge}
    object :~ extends UnapplyGenericLabeledEdge[MyEdge, String]
    object ++ extends UnapplyGenericLabel[String]

    e match {
      case n1 :~ n2 ++ label =>
        val reconstructed = MyEdge(n1, n2, label)
        "reconstructed: MyEdge[Int]" should compile
        reconstructed shouldEqual e
    }
  }

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
      g should have('order(2), 'size(2))

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
      g should have('order(3), 'size(3))
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
      g should have('size (2))
      g.edges foreach { _.label should be(modLabel) }
    }
     */
  }
}
