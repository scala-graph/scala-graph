package scalax.collection

import language.higherKinds

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TMapRootTest
    extends Suites(
      new TMap[immutable.Graph](immutable.Graph),
      new TMap[mutable.Graph](mutable.Graph)
    )

class TMap[CC[N, E[X] <: EdgeLike[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers {

  object `the map of an undirected graph with generic edges` {
    private val edge      = 1 ~ 2
    private val originalG = factory(edge)

    private def fNode(n: originalG.NodeT) = n.outer + 1

    def `yields another graph` {
      val g = originalG map fNode

      g shouldBe a[CC[Int, UnDiEdge] @unchecked]
      g.nodes.head.outer shouldBe an[Integer]
      g.edges.head shouldBe an[g.Inner.UnDiEdge]
      (g.edges.head.outer: UnDiEdge[Int]) shouldBe an[UnDiEdge[_]]
    }
    def `has correctly mapped nodes` {
      val g = originalG map fNode

      originalG.nodes zip g.nodes.toOuter foreach {
        case (original, mapped) => fNode(original) == mapped
      }
    }
    def `has correctly mapped edges` {
      val g = originalG map fNode

      g.edges.head should be(UnDiEdge(2, 3))
    }
    def `may have a new node type` {
      val g = originalG map (_.toString)

      g.nodes.head.outer shouldBe a[String]
      (g.edges.head.outer: UnDiEdge[String]) shouldBe an[UnDiEdge[_]]
      g.edges.head should be(edge._1.toString ~ edge._2.toString)
    }
  }

  object `the map of a directed graph with typed edges works when` {
    private trait Node
    private case class A(a: Int)         extends Node
    private case class B(a: Int, b: Int) extends Node

    private case class ConnectingNodes(source: Node, target: Node)
        extends AbstractDiEdge[Node]
        with PartialEdgeMapper[Node, ConnectingNodes] {
      def map[NN]: PartialFunction[(NN, NN), ConnectingNodes] = {
        case (node_1: Node, node_2: Node) => copy(node_1, node_2)
      }
    }

    private case class ConnectingA(source: A, target: A) extends AbstractDiEdge[A] with PartialEdgeMapper[A, ConnectingA] {
      def map[NN]: PartialFunction[(NN, NN), ConnectingA] = { case (node_1: A, node_2: A) => copy(node_1, node_2) }
    }

    private type EdgeN_P[+N] = AbstractDiEdge[N] with ConnectingNodes
    private type EdgeA_P[+N] = AbstractDiEdge[N] with ConnectingA

    private val a_1 = A(1)
    private val b_0_0 = B(0, 0)

    // TODO apply
    // implicit def fToGeneric(f: F): GenericF[A] = f.asInstanceOf[GenericF[A]]

    def `downcasting nodes ` {
      factory.from[Node, EdgeN_P](edges = ConnectingNodes(a_1, b_0_0) :: Nil) pipe { g =>
        "g.mapBounded(_ => b_0_0): Graph[B, EdgeN_P]" should compile
        //    println(g.map(_.toString + "S", ???): Graph[String, DiEdge])
        /* should not compile
      println(g.map(_.toString + "S"): Graph[String, DiEdge])
       */
      }
    }

    def `may have a new node type` {
      Graph.from[A, EdgeA_P](edges = ConnectingA(a_1, a_1) :: Nil) pipe { g =>
        //    println(g.map(_ => b, EdgeN(_, _)))
        //    println(g.map(_.toString, DiEdge(_, _)))
        /* should not compile
      println(g.map(_ => b, ???): Graph[B, GenericEdgeA])
      println(g.map(_.toString, ???))
       */
      }
    }
  }
}
