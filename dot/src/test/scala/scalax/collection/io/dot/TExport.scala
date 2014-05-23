package scalax.collection
package io.dot

import language.{existentials, implicitConversions}
import scala.collection.SortedMap

import org.scalatest.Spec
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._,
       edge.LDiEdge, edge.Implicits._
import Indent._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Tests [[ArraySet]]. */
@RunWith(classOf[JUnitRunner])
class TExportTest extends Spec with ShouldMatchers {
  def test_Wikipedia {
    // example from http://en.wikipedia.org/wiki/DOT_language
    implicit def toLDiEdge[N](diEdge: DiEdge[N]) = LDiEdge(diEdge._1, diEdge._2)("")
    val g = Graph[String,LDiEdge](
        ("A1"~+>"A2")("f"), ("A2"~+>"A3")("g"),        "A1"~>"B1",
         "A1"~>"B1",        ("A2"~+>"B2")("(g o f)'"), "A3"~>"B3",
         "B1"~>"B3",        ("B2"~+>"B3")("g'"))
    val root = DotRootGraph(directed = true,
                            id       = Some("Wikipedia_Example"))
    val subA = DotSubGraph(ancestor   = root,
                           subgraphId = "A",
                           kvList     = Seq(DotAttr("rank", "same")))
    val subB = DotSubGraph(ancestor   = root,
                           subgraphId = "B",
                           kvList     = Seq(DotAttr("rank", "same")))
    def edgeTransformer(innerEdge: Graph[String,LDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
      val edge = innerEdge.edge
      val label = edge.label.asInstanceOf[String]
      Some(root,
           DotEdgeStmt(edge.from.toString,
                       edge.to.toString,
                       if (label.nonEmpty) List(DotAttr("label", label))
                       else                Nil))
    }
    def nodeTransformer(innerNode: Graph[String,LDiEdge]#NodeT): Option[(DotGraph,DotNodeStmt)] =
      Some((if (innerNode.value.head == 'A') subA else subB,
            DotNodeStmt(innerNode.toString, Seq.empty[DotAttr])))
    val dot = g.toDot(dotRoot          = root,
                      edgeTransformer  = edgeTransformer,
                      cNodeTransformer = Some(nodeTransformer),
                      spacing          = Spacing(TwoSpaces))

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
      (expected_header_sorted + expected_sub_A_sorted + expected_sub_B_sorted + expected_footer,
       expected_header_sorted + expected_sub_B_sorted + expected_sub_A_sorted + expected_footer)
    }
    val dot_sorted = {
      var group = 1
      val groups = {
          val unsortedMap = dot.linesWithSeparators.toList.groupBy { line => group match {
              case 1 | 2 => if (line.contains("subgraph")) group += 1
              case 3     => if (line(0) == '}')            group += 1
              case 4     => line should have length 0
            }
            group
          }
          SortedMap(unsortedMap.toList: _*)
      }
      import scala.math.Ordering.fromLessThan
      val groups_sorted = for (group <- groups.valuesIterator)
        yield group.sorted(
          fromLessThan((a: String, b: String) => {
              val (iA, iB) = (a.indexWhere(_ != ' '), b.indexWhere(_ != ' '))
              if (iA == iB)
                if (a.contains("rank")) true
                else a < b
              else if (a(iA) == '}') false
              else iA < iB
            }
          )
        )
      groups_sorted.flatten
    }.toList.foldLeft("")((result: String, elem: String) => result + elem)
    dot_sorted should (be (expected_1) or
                       be (expected_2))
  }
  def test_header {
     val g = Graph.empty[String, UnDiEdge]
     val dot = g.toDot(
         dotRoot = DotRootGraph (directed = false,
                                 id       = None,
                                 kvList   = Seq(DotAttr("attr_1", """"one""""),
                                                DotAttr("attr_2", "<two>"))),
         edgeTransformer  = _ => None,
         spacing          = Spacing(TwoSpaces))
    val expected = """graph {
      |  attr_1 = "one"
      |  attr_2 = <two>
      |}""".stripMargin
    dot should be (expected)
  }
}