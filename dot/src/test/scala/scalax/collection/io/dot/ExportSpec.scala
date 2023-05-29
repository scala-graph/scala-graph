package scalax.collection
package io.dot

import scala.collection.SortedMap

import scalax.collection.OuterImplicits._
import scalax.collection.edges.{DiEdge, DiEdgeImplicits, UnDiEdge}
import scalax.collection.edges.labeled.LDiEdge
import scalax.collection.hyperedges._
import scalax.collection.immutable.{Graph, TypedGraphFactory}
import scalax.collection.io.dot.Indent._

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class ExportSpec extends RefSpec with Matchers {
  import ExportSpec.Lines

  def `Example at http://en.wikipedia.org/wiki/DOT_language is produced`(): Unit = {
    import ExportSpec.wikipedia._
    import ExportSpec.wikipedia.ExampleGraph.OuterImplicits._

    val g = ExampleGraph(
      "A1" ~> "A2" :+ "f",
      "A2" ~> "A3" :+ "g",
      "A1" ~> "B1" :+ "",
      "A1" ~> "B1" :+ "",
      "A2" ~> "B2" :+ "(g o f)'",
      "A3" ~> "B3" :+ "",
      "B1" ~> "B3" :+ "",
      "B2" ~> "B3" :+ "g'"
    )
    val root = DotRootGraph(directed = true, id = Some(Id("Wikipedia_Example")))
    val subA = DotSubGraph(ancestor = root, subgraphId = Id("A"), attrList = List(DotAttr(Id("rank"), Id("same"))))
    val subB = DotSubGraph(ancestor = root, subgraphId = Id("B"), attrList = List(DotAttr(Id("rank"), Id("same"))))

    def edgeTransformer(innerEdge: ExampleGraph#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      val edge  = innerEdge.outer
      val label = edge.label
      Some(
        root,
        DotEdgeStmt(
          NodeId(edge.source),
          NodeId(edge.target),
          if (label.nonEmpty) List(DotAttr(Id("label"), Id(label)))
          else Nil
        )
      )
    }

    def nodeTransformer(innerNode: ExampleGraph#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some(
        (if (innerNode.outer.head == 'A') subA else subB, DotNodeStmt(NodeId(innerNode.toString), Seq.empty[DotAttr]))
      )

    val dot = g.toDot(
      dotRoot = root,
      edgeTransformer = edgeTransformer,
      cNodeTransformer = Some(nodeTransformer),
      spacing = multilineCompatibleSpacing
    )

    val (expected_1, expected_2) = {
      val expected_header_sorted =
        """digraph Wikipedia_Example {
          |  A1 -> A2 [label = f]
          |  A1 -> B1
          |  A2 -> A3 [label = g]
          |  A2 -> B2 [label = "(g o f)'"]
          |  A3 -> B3
          |  B1 -> B3
          |  B2 -> B3 [label = "g'"]""".stripMargin
      val expected_footer = """
                              |}""".stripMargin
      val expected_sub_A_sorted = """
                                    |  subgraph A {
                                    |    A1
                                    |    A2
                                    |    A3
                                    |    rank = same
                                    |  }""".stripMargin
      val expected_sub_B_sorted = """
                                    |  subgraph B {
                                    |    B1
                                    |    B2
                                    |    B3
                                    |    rank = same
                                    |  }""".stripMargin
      (
        expected_header_sorted + expected_sub_A_sorted + expected_sub_B_sorted + expected_footer,
        expected_header_sorted + expected_sub_B_sorted + expected_sub_A_sorted + expected_footer
      )
    }
    val dot_sorted = {
      var group = 1
      val groups = {
        val unsortedMap = dot.linesWithSeparators.toList.groupBy { line =>
          group match {
            case 1 | 2 => if (line.contains("subgraph")) group += 1
            case 3     => if (line.head == '}') group += 1
            case 4     => line should have length 0
          }
          group
        }
        SortedMap(unsortedMap.toList: _*)
      }
      import scala.math.Ordering.fromLessThan
      val groups_sorted =
        for (group <- groups.valuesIterator)
          yield group.sorted(
            fromLessThan { (a: String, b: String) =>
              val (iA, iB) = (a.indexWhere(_ != ' '), b.indexWhere(_ != ' '))
              if (iA == iB)
                if (a.contains("rank")) true
                else a < b
              else if (a(iA) == '}') false
              else iA < iB
            }
          )
      groups_sorted.flatten
    }.toList.foldLeft("")((result: String, elem: String) => result + elem)
    dot_sorted should (be(expected_1) or
      be(expected_2))
  }

  def `DOT headers are covered even in edge cases`(): Unit =
    Graph
      .empty[String, UnDiEdge[String]]
      .toDot(
        dotRoot = DotRootGraph(
          directed = false,
          id = None,
          attrList = List(
            DotAttr(Id("attr_1"), Id(""""one"""")),
            DotAttr(Id("attr_2"), Id("<two>"))
          )
        ),
        edgeTransformer = _ => None,
        spacing = multilineCompatibleSpacing
      ) shouldBe
      """graph {
        |  attr_1 = "one"
        |  attr_2 = <two>
        |}""".stripMargin

  def `Directed hyperedges may be mapped to multiple directed DOT edges`(): Unit = {
    val hg   = Graph.from(OneOrMore(1) ~~> OneOrMore(2, 3) :: Nil)
    val root = DotRootGraph(directed = true, id = None)
    val dot = hg.toDot(
      dotRoot = root,
      edgeTransformer = _ => None,
      hEdgeTransformer = Some { h =>
        val source = h.outer.sources.head.toString
        h.outer.targets.toList map (target => (root, DotEdgeStmt(NodeId(source), NodeId(target.toString))))
      },
      spacing = multilineCompatibleSpacing
    )

    Lines(dot) shouldBe Lines(
      """digraph {
        |  1 -> 2
        |  1 -> 3
        |}""".stripMargin
    )
  }

  def `Colons in https://www.graphviz.org/doc/info/shapes.html#record are handled correctly`(): Unit = {
    import implicits._, Record._
    import ExportSpec.records._
    import ExportSpec.records.RecordGraph.OuterImplicits._

    def struct(i: Int)     = s"struct$i"
    val (f0, f1, f2, here) = ("f0", "f1", "f2", "here")
    val (n1, n2, n3): (Node, Node, Node) = (
      Node(
        struct(1),
        Horizontal(Field("left", Some(f0)), Horizontal(Field("mid", Some(f1)), Field("right", Some(f2))))
      ),
      Node(struct(2), Horizontal(Field("one", Some(f0)), Field("two", Some(f1)))),
      Node(
        struct(3),
        Horizontal(
          Field("hello&#92;nworld"),
          Horizontal(
            Vertical(
              Horizontal(Field("b"), Vertical(Field("c"), Horizontal(Field("d", Some(here)), Field("e")))),
              Field("f")
            ),
            Horizontal(Field("g"), Field("h"))
          )
        )
      )
    )
    val g = RecordGraph(
      n1 ~> n2 :+ Ports(f1, f0),
      n1 ~> n3 :+ Ports(f2, here)
    )
    val root = DotRootGraph(
      directed = true,
      id = Some("structs"),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record"))))
    )
    val dot = g.toDot(
      dotRoot = root,
      edgeTransformer = _.outer match {
        case RecordArrow(source, target, ports) =>
          def withPort(n: Node, port: String) = NodeId(n.id, port)
          Some(
            (root, DotEdgeStmt(withPort(source, ports.port_1), withPort(target, ports.port_2)))
          )
      },
      cNodeTransformer = Some(_.outer match {
        case Node(id, label) => Some((root, DotNodeStmt(id, List(DotAttr("label", label.toString)))))
      }),
      spacing = multilineCompatibleSpacing
    )

    Lines(dot) shouldBe Lines(
      """digraph structs {
        |  node [shape = record]
        |  struct1 [label = "<f0> left | <f1> mid | <f2> right"]
        |  struct1:f1 -> struct2:f0
        |  struct1:f2 -> struct3:here
        |  struct2 [label = "<f0> one | <f1> two"]
        |  struct3 [label = "hello&#92;nworld | {b | {c | <here> d | e} | f} | g | h"]
        |}""".stripMargin
    )
  }

  def `doubly-nested subgraphs #69`(): Unit = {
    import scalax.collection.edges.DiEdge
    import scalax.collection.io.dot.implicits._

    val g = Graph[Int, DiEdge](1)
    val root = DotRootGraph(
      directed = true,
      id = Some("structs")
    )
    val branchDOT = DotSubGraph(root, "cluster_branch", attrList = List(DotAttr("label", "branch")))
    val cSubGraph = DotSubGraph(branchDOT, "cluster_chained", attrList = List(DotAttr("label", "Chained")))
    val iSubGraph = DotSubGraph(branchDOT, "cluster_unchained", attrList = List(DotAttr("label", "UnChained")))
    val iNode     = "inode"
    val dot = g.toDot(
      dotRoot = root,
      edgeTransformer = _ => Some((root, DotEdgeStmt("hi", "guys"))),
      cNodeTransformer = Some(_ => Some((cSubGraph, DotNodeStmt("cnode")))),
      iNodeTransformer = Some(_ => Some((iSubGraph, DotNodeStmt(iNode))))
    )
    dot.contains(iNode) shouldBe true
  }

  private val multilineCompatibleSpacing = Spacing(
    indent = TwoSpaces,
    graphAttrSeparator = new AttrSeparator("""
                                             |""".stripMargin) {}
  )
}

private object ExportSpec {
  object wikipedia {
    case class LDiEdgeOfStrings(source: String, target: String, label: String) extends LDiEdge[String, String]

    implicit class InfixConstructor(val e: DiEdge[String]) extends AnyVal {
      def :+(label: String) = LDiEdgeOfStrings(e.source, e.target, label)
    }

    type ExampleGraph = Graph[String, LDiEdgeOfStrings]
    object ExampleGraph extends TypedGraphFactory[String, LDiEdgeOfStrings]
  }

  object records {
    import Record.Ports

    case class Node(id: Id, label: Record.RLabel)

    case class RecordArrow(source: Node, target: Node, label: Ports) extends LDiEdge[Node, Ports]

    implicit class InfixConstructor(val e: DiEdge[Node]) extends AnyVal {
      def :+(ports: Ports) = RecordArrow(e.source, e.target, ports)
    }

    type RecordGraph = Graph[Node, RecordArrow]
    object RecordGraph extends TypedGraphFactory[Node, RecordArrow]
  }

  case class Lines(head: String, mid: Set[String], tail: String)
  object Lines {
    def apply(dot: String): Lines = dot.linesWithSeparators.toList match {
      case head :: second :: tail =>
        Lines(head, (second :: tail.init).toSet, tail.last)
      case _ => throw new IllegalArgumentException("Dot string of at least three lines expected.")
    }
  }
}
