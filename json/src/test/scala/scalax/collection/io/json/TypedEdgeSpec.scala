package scalax.collection
package io.json

import net.liftweb.json._

import scalax.collection.AnyGraph
import scalax.collection.generic._
import descriptor._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.Suites

import serializer._

class TypedEdgeSpec
    extends Suites(
      new CustomEdge[immutable.Graph](immutable.Graph),
      new CustomEdge[mutable.Graph](mutable.Graph)
    )

private class CustomEdge[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  object `JSON import/export of graphs with typed edges` {
    import CustomEdge.typedEdge._
    import KeyModifier._

    private val jsonText = """{
      "nodes" : [
        ["Editor"],
        ["Menu"],
        ["Settings"]
      ],
      "edges": [
        ["Editor", "Menu",     ["M", "Alt"] ],
        ["Menu",   "Settings", ["S", "NoneModifier"]]
        ]
    }""".filterNot(_.isWhitespace)

    private val graph = factory.from[String, Transition](
      nodes = Nil,
      edges = Transition("Editor", "Menu", 'M', Alt) ::
        Transition("Menu", "Settings", 'S', NoneModifier) ::
        Nil
    )

    private val descriptor = {
      val keyModifierSerializer = new PositionedSerializer[KeyModifier](
        { case JString(mod) => KeyModifier.withName(mod) },
        { case mod: KeyModifier => JString(mod.toString) }
      )
      new Descriptor[String](
        StringNodeDescriptor,
        new LEdgeDescriptor[String, Transition, (Char, KeyModifier)](
          (
              from: String,
              to: String,
              label: (Char, KeyModifier)
          ) => Transition(from, to, label._1, label._2),
          aLabel = ('A', NoneModifier),
          customSerializer = Some(
            new LEdgeSerializer[(Char, KeyModifier)](
              new Tuple2Serializer[Char, KeyModifier](CharSerializer, keyModifierSerializer)
            )
          ),
          typeId = "TraceEdge"
        ) {
          protected def label(edge: Transition): (Char, KeyModifier) = (edge.key, edge.keyMod)
        }
      )
    }

    def `on import`(): Unit = factory.fromJson[String, Transition](jsonText, descriptor) shouldBe graph
    def `on reimport`(): Unit =
      factory.fromJson[String, Transition](graph.toJson(descriptor), descriptor) shouldBe graph
  }

  object `JSON import/export of hypergraphs ` {
    object `with typed hyperedges` {
      import CustomEdge.WLHyperEdge

      private val jsonText = s"""
            { "nodes" : [["A"], ["B"], ["C"]],
              "edges": [
                [{"ends": ["A", "B"]},      {"labels": [10.1, "Label 1"]}],
                [{"ends": ["B", "A", "C"]}, {"labels": [20.2, "Label 2"]}]
              ]
            }""".filterNot(_.isWhitespace)

      private val graph = factory.from[String, WLHyperEdge](
        nodes = List("A", "B", "C"),
        edges = List(
          WLHyperEdge(Several("A", "B"), 10.1, "Label 1"),
          WLHyperEdge(Several("A", "B", "C"), 20.2, "Label 2")
        )
      )

      private val descriptor: Descriptor[String] =
        new Descriptor[String](
          StringNodeDescriptor,
          new LHyperEdgeDescriptor[String, WLHyperEdge, (Double, String)](
            (ends: Several[String], label: (Double, String)) => WLHyperEdge(ends, label._1, label._2),
            (Double.MaxValue, ""),
            Some(
              new LHyperEdgeSerializer[(Double, String)](
                new Tuple2Serializer[Double, String](DoubleSerializer, StringSerializer)
              )
            )
          ) {
            protected def label(edge: WLHyperEdge): (Double, String) = (edge.weight, edge.label)
          }
        )

      def `on import`(): Unit = factory.fromJson[String, WLHyperEdge](jsonText, descriptor) shouldBe graph
      def `on reimport`(): Unit =
        factory.fromJson[String, WLHyperEdge](graph.toJson(descriptor), descriptor) shouldBe graph
    }

    object `with typed directed hyperedges` {
      import CustomEdge.WLDiHyperEdge

      private val jsonText = s"""
        { "nodes" : [["A"], ["B"], ["C"]],
          "edges": [
            [{"sources": ["A", "B"]}, {"targets": ["A", "B", "C"]}, {"labels": [10.1, "Label 1"]}],
            [{"sources": ["B", "A", "C"]}, {"targets": ["A", "B"]}, {"labels": [20.2, "Label 2"]}]
          ]
        }""".filterNot(_.isWhitespace)

      private val graph = factory.from[String, WLDiHyperEdge](
        nodes = List("A", "B", "C"),
        edges = List(
          WLDiHyperEdge(OneOrMore("A", "B"), OneOrMore("A", "B", "C"), 10.1, "Label 1"),
          WLDiHyperEdge(OneOrMore("A", "B", "C"), OneOrMore("A", "B"), 20.2, "Label 2")
        )
      )

      private val descriptor: Descriptor[String] =
        new Descriptor[String](
          StringNodeDescriptor,
          new LDiHyperEdgeDescriptor[String, WLDiHyperEdge, (Double, String)](
            (sources: OneOrMore[String], targets: OneOrMore[String], label: (Double, String)) =>
              WLDiHyperEdge(sources, targets, label._1, label._2),
            (Double.MaxValue, ""),
            Some(
              new LDiHyperEdgeSerializer[(Double, String)](
                new Tuple2Serializer[Double, String](DoubleSerializer, StringSerializer)
              )
            )
          ) {
            protected def label(edge: WLDiHyperEdge): (Double, String) = (edge.weight, edge.label)
          }
        )

      def `on import`(): Unit = factory.fromJson[String, WLDiHyperEdge](jsonText, descriptor) shouldBe graph
      def `on reimport`(): Unit =
        factory.fromJson[String, WLDiHyperEdge](graph.toJson(descriptor), descriptor) shouldBe graph
    }
  }
}

private object CustomEdge {

  protected object typedEdge {
    object KeyModifier extends Enumeration {
      type KeyModifier = Value
      val NoneModifier, Alt, Ctrl, Shift = Value
    }
    import KeyModifier._

    case class Transition(from: String, to: String, val key: Char, val keyMod: KeyModifier)
        extends AbstractDiEdge[String](from, to)
        with MultiEdge {

      def extendKeyBy: OneOrMore[Any] = OneOrMore(key, keyMod)
    }
  }

  case class WLHyperEdge(override val ends: Several[String], override val weight: Double, label: String)
      extends AbstractHyperEdge(ends)

  case class WLDiHyperEdge(
      override val sources: OneOrMore[String],
      override val targets: OneOrMore[String],
      override val weight: Double,
      label: String
  ) extends AbstractDiHyperEdge(sources, targets)
}
