package scalax.collection.io.json
package descriptor
package predefined

import net.liftweb.json._

import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.io.edge._

trait PredefinedEdgeDescriptorBase {
  def typeId = this.toString + "Edge"
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
trait PredefinedLEdgeDescriptor extends PredefinedEdgeDescriptorBase {
  def descriptor[N, L <: AnyRef: Manifest](
      aLabel: L,
      customSerializer: Option[Serializer[_ <: Parameters]] = None): GenEdgeDescriptor[N]
}
case object UnDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new EdgeDescriptor[N, UnDiEdge, UnDiEdge.type](UnDiEdge, check[EdgeParameters](customSerializer), Nil, typeId)
}
case object WUnDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WEdgeDescriptor[N, WUnDiEdge, WUnDiEdge.type](WUnDiEdge, check[WEdgeParameters](customSerializer), Nil, typeId)
}
case object WkUnDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WEdgeDescriptor[N, WkUnDiEdge, WkUnDiEdge.type](
      WkUnDiEdge,
      check[WEdgeParameters](customSerializer),
      Nil,
      typeId)
}
case object LUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LEdgeDescriptor[N, LUnDiEdge, LUnDiEdge.type, L](
      LUnDiEdge,
      aLabel,
      check[LEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object LkUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LEdgeDescriptor[N, LkUnDiEdge, LkUnDiEdge.type, L](
      LkUnDiEdge,
      aLabel,
      check[LEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WLUnDiEdge, WLUnDiEdge.type, L](
      WLUnDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WkLUnDiEdge, WkLUnDiEdge.type, L](
      WkLUnDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLkUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WLkUnDiEdge, WLkUnDiEdge.type, L](
      WLkUnDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLkUnDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WkLkUnDiEdge, WkLkUnDiEdge.type, L](
      WkLkUnDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object Di extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new EdgeDescriptor[N, DiEdge, DiEdge.type](DiEdge, check(customSerializer), Nil, typeId)
}
case object WDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WEdgeDescriptor[N, WDiEdge, WDiEdge.type](WDiEdge, check(customSerializer), Nil, typeId)
}
case object WkDi extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WEdgeDescriptor[N, WkDiEdge, WkDiEdge.type](WkDiEdge, check(customSerializer), Nil, typeId)
}
case object LDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LEdgeDescriptor[N, LDiEdge, LDiEdge.type, L](
      LDiEdge,
      aLabel,
      check[LEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object LkDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LEdgeDescriptor[N, LkDiEdge, LkDiEdge.type, L](
      LkDiEdge,
      aLabel,
      check[LEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WLDiEdge, WLDiEdge.type, L](
      WLDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WkLDiEdge, WkLDiEdge.type, L](
      WkLDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLkDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WLkDiEdge, WLkDiEdge.type, L](
      WLkDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLkDi extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLEdgeDescriptor[N, WkLkDiEdge, WkLkDiEdge.type, L](
      WkLkDiEdge,
      aLabel,
      check[WLEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object Hyper extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new HyperEdgeDescriptor[N, HyperEdge, HyperEdge.type](
      HyperEdge,
      check[HyperEdgeParameters](customSerializer),
      Nil,
      typeId)
}
//case object WHyper extends PredefinedEdgeDescriptor {
//  override def descriptor[N](customSerializer: Option[Serializer[_<:Parameters]] = None) =
//    new WHyperEdgeDescriptor[N,WHyperEdge,WHyperEdge.type](
//        WHyperEdge, check[WHyperEdgeParameters](customSerializer))
//}
//case object WkHyper extends PredefinedEdgeDescriptor {
//  override def descriptor[N](customSerializer: Option[Serializer[_<:Parameters]] = None) =
//    new WHyperEdgeDescriptor[N,WkHyperEdge,WkHyperEdge.type](
//        WkHyperEdge, check[WHyperEdgeParameters](customSerializer))
//}
case object LHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LHyperEdgeDescriptor[N, LHyperEdge, LHyperEdge.type, L](
      LHyperEdge,
      aLabel,
      check[LHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object LkHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LHyperEdgeDescriptor[N, LkHyperEdge, LkHyperEdge.type, L](
      LkHyperEdge,
      aLabel,
      check[LHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WLHyperEdge, WLHyperEdge.type, L](
      WLHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WkLHyperEdge, WkLHyperEdge.type, L](
      WkLHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLkHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WLkHyperEdge, WLkHyperEdge.type, L](
      WLkHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLkHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WkLkHyperEdge, WkLkHyperEdge.type, L](
      WkLkHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object DiHyper extends PredefinedEdgeDescriptor {
  override def descriptor[N](customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new HyperEdgeDescriptor[N, DiHyperEdge, DiHyperEdge.type](DiHyperEdge, check(customSerializer), Nil, typeId)
}
//case object WDiHyper extends PredefinedEdgeDescriptor {
//  override def descriptor[N](customSerializer: Option[Serializer[_<:Parameters]] = None) =
//    new WHyperEdgeDescriptor[N,WDiHyperEdge,WDiHyperEdge.type](
//        WDiHyperEdge, check(customSerializer))
//}
//case object WkDiHyper extends PredefinedEdgeDescriptor {
//  override def descriptor[N](customSerializer: Option[Serializer[_<:Parameters]] = None) =
//    new WHyperEdgeDescriptor[N,WkDiHyperEdge,WkDiHyperEdge.type](
//        WkDiHyperEdge, check(customSerializer))
//}
case object LDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LHyperEdgeDescriptor[N, LDiHyperEdge, LDiHyperEdge.type, L](
      LDiHyperEdge,
      aLabel,
      check[LHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object LkDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new LHyperEdgeDescriptor[N, LkDiHyperEdge, LkDiHyperEdge.type, L](
      LkDiHyperEdge,
      aLabel,
      check[LHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WLDiHyperEdge, WLDiHyperEdge.type, L](
      WLDiHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WkLDiHyperEdge, WkLDiHyperEdge.type, L](
      WkLDiHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WLkDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WLkDiHyperEdge, WLkDiHyperEdge.type, L](
      WLkDiHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
case object WkLkDiHyper extends PredefinedLEdgeDescriptor {
  override def descriptor[N, L <: AnyRef: Manifest](aLabel: L,
                                                    customSerializer: Option[Serializer[_ <: Parameters]] = None) =
    new WLHyperEdgeDescriptor[N, WkLkDiHyperEdge, WkLkDiHyperEdge.type, L](
      WkLkDiHyperEdge,
      aLabel,
      check[WLHyperEdgeParameters[L]](customSerializer),
      Nil,
      typeId)
}
