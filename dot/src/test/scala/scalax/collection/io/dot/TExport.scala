package scalax.collection
package io.dot

import language.implicitConversions
import scala.collection.SortedMap

import GraphPredef._, GraphEdge._, edge.LDiEdge, edge.Implicits._
import Indent._

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
/** Tests [[Export]]. */
class TExportTest extends RefSpec with Matchers {

  def `Example at http://en.wikipedia.org/wiki/DOT_language will be produced` {

    implicit def toLDiEdge[N](diEdge: DiEdge[N]) = LDiEdge(diEdge._1, diEdge._2)("")
    val g = Graph[String, LDiEdge](
      ("A1" ~+> "A2")("f"),
      ("A2" ~+> "A3")("g"),
      "A1" ~> "B1",
      "A1" ~> "B1",
      ("A2" ~+> "B2")("(g o f)'"),
      "A3" ~> "B3",
      "B1" ~> "B3",
      ("B2" ~+> "B3")("g'"))
    val root = DotRootGraph(directed = true, id = Some(Id("Wikipedia_Example")))
    val subA = DotSubGraph(ancestor = root, subgraphId = Id("A"), attrList = List(DotAttr(Id("rank"), Id("same"))))
    val subB = DotSubGraph(ancestor = root, subgraphId = Id("B"), attrList = List(DotAttr(Id("rank"), Id("same"))))
    def edgeTransformer(innerEdge: Graph[String, LDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      val edge  = innerEdge.edge
      val label = edge.label.asInstanceOf[String]
      Some(
        root,
        DotEdgeStmt(
          NodeId(edge.from.toString),
          NodeId(edge.to.toString),
          if (label.nonEmpty) List(DotAttr(Id("label"), Id(label)))
          else Nil))
    }
    def nodeTransformer(innerNode: Graph[String, LDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some(
        (if (innerNode.value.head == 'A') subA else subB, DotNodeStmt(NodeId(innerNode.toString), Seq.empty[DotAttr])))
    val dot = g.toDot(
      dotRoot = root,
      edgeTransformer = edgeTransformer,
      cNodeTransformer = Some(nodeTransformer),
      spacing = multilineCompatibleSpacing)

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
      val expected_footer       = """
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
        expected_header_sorted + expected_sub_B_sorted + expected_sub_A_sorted + expected_footer)
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
      val groups_sorted = for (group <- groups.valuesIterator)
        yield
          group.sorted(
            fromLessThan((a: String, b: String) => {
              val (iA, iB) = (a.indexWhere(_ != ' '), b.indexWhere(_ != ' '))
              if (iA == iB)
                if (a.contains("rank")) true
                else a < b
              else if (a(iA) == '}') false
              else iA < iB
            })
          )
      groups_sorted.flatten
    }.toList.foldLeft("")((result: String, elem: String) => result + elem)
    dot_sorted should (be(expected_1) or
      be(expected_2))
  }

  def `DOT headers are covered even in edge cases` {
    val g = Graph.empty[String, UnDiEdge]
    val dot = g.toDot(
      dotRoot = DotRootGraph(
        directed = false,
        id = None,
        attrList = List(DotAttr(Id("attr_1"), Id(""""one"""")), DotAttr(Id("attr_2"), Id("<two>")))),
      edgeTransformer = _ => None,
      spacing = multilineCompatibleSpacing
    )
    val expected = """graph {
                     |  attr_1 = "one"
                     |  attr_2 = <two>
                     |}""".stripMargin
    dot should be(expected)
  }

  def `Directed hyperedges may be mapped to multiple directed DOT edges` {
    val hg   = Graph(1 ~> 2 ~> 3)
    val root = DotRootGraph(directed = true, id = None)
    val dot = hg.toDot(
      dotRoot = root,
      edgeTransformer = e => None,
      hEdgeTransformer = Some(h => {
        val source = h.edge.source.toString
        h.edge.targets.toTraversable map (target => (root, DotEdgeStmt(NodeId(source), NodeId(target.toString))))
      }),
      spacing = multilineCompatibleSpacing
    )
    val expected = """digraph {
                     |  1 -> 2
                     |  1 -> 3
                     |}""".stripMargin
    sortMid(dot) should be(expected)
  }

  def `Colons (':') in node_id's are handeled correctly` {
    def struct(i: Int) = s"struct$i"
    import implicits._, Record._
    val (f0, f1, f2, here) = ("f0", "f1", "f2", "here")
    val (n1, n2, n3): (Node, Node, Node) = (
      Node(
        struct(1),
        Horizontal(Field("left", Some(f0)), Horizontal(Field("mid", Some(f1)), Field("right", Some(f2))))),
      Node(struct(2), Horizontal(Field("one", Some(f0)), Field("two", Some(f1)))),
      Node(
        struct(3),
        Horizontal(
          Field("hello&#92;nworld"),
          Horizontal(
            Vertical(
              Horizontal(Field("b"), Vertical(Field("c"), Horizontal(Field("d", Some(here)), Field("e")))),
              Field("f")),
            Horizontal(Field("g"), Field("h")))
        )
      )
    )
    val g = Graph((n1 ~+> n2)(Ports(f1, f0)), (n1 ~+> n3)(Ports(f2, here)))
    val root = DotRootGraph(
      directed = true,
      id = Some("structs"),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record"))))
    )
    val dot = g.toDot(
      dotRoot = root,
      edgeTransformer = _.edge match {
        case LDiEdge(source, target, label) =>
          def withPort(n: Node, port: String): NodeId = n match {
            case Node(id, _) => NodeId(id, port)
          }
          label match {
            case Ports(sourcePort, targetPort) =>
              Some((root, DotEdgeStmt(withPort(source.value, sourcePort), withPort(target.value, targetPort)))): Option[
                (DotGraph, DotEdgeStmt)]
          }
      },
      cNodeTransformer = Some(_.value match {
        case Node(id, label) =>
          Some((root, DotNodeStmt(id, List(DotAttr("label", label.toString)))))
      }),
      spacing = multilineCompatibleSpacing
    )

    val expected = """digraph structs {
                     |  node [shape = record]
                     |  struct1 [label = "<f0> left | <f1> mid | <f2> right"]
                     |  struct1:f1 -> struct2:f0
                     |  struct1:f2 -> struct3:here
                     |  struct2 [label = "<f0> one | <f1> two"]
                     |  struct3 [label = "hello&#92;nworld | {b | {c | <here> d | e} | f} | g | h"]
                     |}""".stripMargin
    sortMid(dot) should be(expected)
  }

  def `doubly-nested subgraphs #69` {
    import scalax.collection.Graph
    import scalax.collection.GraphEdge.DiEdge
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
      edgeTransformer = _.edge match {
        case _ =>
          Some((root, DotEdgeStmt("hi", "guys")))
      },
      cNodeTransformer = Some({ _ =>
        Some((cSubGraph, DotNodeStmt("cnode")))
      }),
      iNodeTransformer = Some({ _ =>
        Some((iSubGraph, DotNodeStmt(iNode)))
      })
    )
    dot.contains(iNode) should be(true)
  }

  private val multilineCompatibleSpacing = Spacing(
    indent = TwoSpaces,
    graphAttrSeparator = new AttrSeparator("""
                                             |""".stripMargin) {})

  private def sortMid(dot: String): String = {
    val lines = dot.linesWithSeparators.toBuffer
    val mid   = lines.tail.init
    s"${lines.head}${mid.sorted.mkString}${lines.last}"
  }
}

case class Node(id: Id, label: Record.RLabel)
