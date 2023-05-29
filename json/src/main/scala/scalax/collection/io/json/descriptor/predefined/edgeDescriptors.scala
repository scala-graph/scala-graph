package scalax.collection.io
package json
package descriptor
package predefined

import net.liftweb.json._

import scalax.collection.edges._
import scalax.collection.edges.labeled.{WDiEdge, WUnDiEdge}
import scalax.collection.edges.multilabeled.{WDiEdge => MultiWDiEdge, WUnDiEdge => MultiWUnDiEdge}
import scalax.collection.hyperedges.{DiHyperEdge, HyperEdge}

import edge._
import serializer._

trait PredefinedEdgeDescriptorBase {
  def caseObjectBasedTypeId = this.toString + "Edge"
  /* this check is currently of no value since T/Serializer[T] is unchecked */
  protected def check[P](serializer: Option[Serializer[_ <: Parameters]]) =
    serializer match {
      case ser: Option[Serializer[P] @unchecked] => ser
      case _                                     => throw new IllegalArgumentException
    }
}

trait PredefinedEdgeDescriptor extends PredefinedEdgeDescriptorBase {
  def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None): GenEdgeDescriptor[N]
}

case object UnDi extends PredefinedEdgeDescriptor {
  def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new EdgeDescriptor[N, UnDiEdge[N]](
      UnDiEdge.apply,
      check[EdgeParameters](customSerializer),
      Nil,
      caseObjectBasedTypeId
    )
}

case object WUnDi extends PredefinedEdgeDescriptor {
  def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.wEdgeSerializer) =
    new LEdgeDescriptor[N, WUnDiEdge[N], Double](
      WUnDiEdge.apply,
      Double.MaxValue,
      check[LEdgeParameters[Double]](customSerializer),
      Nil,
      caseObjectBasedTypeId
    ) {
      protected def label(edge: WUnDiEdge[N]): Double = edge.weight
    }
}

case object MultiWUnDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.wEdgeSerializer) =
    new LEdgeDescriptor[N, MultiWUnDiEdge[N], Double](
      MultiWUnDiEdge.apply,
      Double.MaxValue,
      check[LEdgeParameters[Double]](customSerializer),
      Nil,
      caseObjectBasedTypeId
    ) {
      protected def label(edge: MultiWUnDiEdge[N]): Double = edge.weight
    }
}

case object Di extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new EdgeDescriptor[N, DiEdge[N]](DiEdge.apply, check(customSerializer), Nil, caseObjectBasedTypeId)
}

case object WDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.wEdgeSerializer) =
    new LEdgeDescriptor[N, WDiEdge[N], Double](
      WDiEdge.apply,
      Double.MaxValue,
      check[LEdgeParameters[Double]](customSerializer),
      Nil,
      caseObjectBasedTypeId
    ) {
      protected def label(edge: WDiEdge[N]): Double = edge.weight
    }
}

case object MultiWDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.wEdgeSerializer) =
    new LEdgeDescriptor[N, MultiWDiEdge[N], Double](
      MultiWDiEdge.apply,
      Double.MaxValue,
      check[LEdgeParameters[Double]](customSerializer),
      Nil,
      caseObjectBasedTypeId
    ) {
      protected def label(edge: MultiWDiEdge[N]): Double = edge.weight
    }
}

case object Hyper extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.hyperEdgeSerializer) =
    new HyperEdgeDescriptor[N, HyperEdge[N]](
      HyperEdge.apply,
      check[HyperEdgeParameters](customSerializer),
      Nil,
      caseObjectBasedTypeId
    )
}

case object DiHyper extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = some.diHyperEdgeSerializer) =
    new DiHyperEdgeDescriptor[N, DiHyperEdge[N]](DiHyperEdge.apply, check(customSerializer), Nil, caseObjectBasedTypeId)
}

private object some {
  val wEdgeSerializer       = Some(new LEdgeSerializer(DoubleSerializer))
  val hyperEdgeSerializer   = Some(new HyperEdgeSerializer)
  val diHyperEdgeSerializer = Some(new DiHyperEdgeSerializer)
}
