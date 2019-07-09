package scalax.collection.io.json
package descriptor

import language.higherKinds
import reflect.ClassTag

import net.liftweb.json._

import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge._, scalax.collection.edge.WBase._, scalax.collection.edge.LBase._,
scalax.collection.edge.WLBase._, scalax.collection.edge.CBase._
import scalax.collection.io.edge._

/** Generic base trait for any `*EdgeDescriptor` excluding edge types
  * to be used as type argument to collections containing edge descriptors
  * of different types. */
sealed abstract class GenEdgeDescriptor[N](val edgeManifest: ClassTag[_], override val typeId: String)
    extends TypeId(typeId)

/** Base trait for any `class *EdgeDescriptor`.
  *
  * @define USAGE Instances of this class must be passed as a constructor argument to a
  *         [[scalax.collection.io.json.descriptor.Descriptor]] either directly or
  *         indirectly by utilizing a predefined edge descriptor.
  * @define COMPANION an edge companion object such as
  * @define CUSTSER `Some` list-json custom serializer or `None`.
  * @define EXTRACL list of classes referenced by this node provided they are to be
  *        (de)serialized; empty by default.
  * @define TYPEID denotes the edge type in a JSON text.
  * @define LABEL any sample value for the label type `L` such as `""` for `String`.
  * @define ATTR any sample value for the `Product` of custom attributes.
  *
  * @param edgeCompanion  $COMPANION `UnDiEdge` or `LWHyperEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  *
  * @author Peter Empen
  */
sealed abstract class EdgeDescriptorBase[N, E[X] <: EdgeLikeIn[X], +C <: EdgeCompanionBase[E]](
    val edgeCompanion: C,
    val customSerializer: Option[Serializer[_ <: Parameters]] = None,
    val extraClasses: List[Class[_]] = Nil,
    typeId: String)(implicit edgeManifest: ClassTag[E[N]])
    extends GenEdgeDescriptor[N](edgeManifest, typeId) {
  implicit val formats = Serialization.formats(
    if (extraClasses.isEmpty) NoTypeHints
    else new ShortTypeHints(extraClasses)) ++ customSerializer
  def extract(jsonEdge: JValue): Parameters
  final def decompose(edge: E[N])(implicit descriptor: Descriptor[N]): JValue =
    Extraction.decompose(toParameters(edge))
  protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]): Parameters
  protected def nodeIds(edge: E[N], descriptor: Descriptor[N]) =
    edge.iterator.toList map (n => descriptor.nodeDescriptor(n) id (n))
}

/** Determines how to extract data relevant for non-weighted,
  * non-labeled edges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `UnDiEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class EdgeDescriptor[N, E[X] <: UnDiEdge[X], +C <: EdgeCompanion[E]](
    edgeCompanion: C,
    customSerializer: Option[Serializer[_ <: EdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[EdgeParameters]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) = {
    val (n1, n2) = (edge._1, edge._2)
    new EdgeParameters(descriptor.nodeDescriptor(n1) id (n1), descriptor.nodeDescriptor(n2) id (n2))
  }
}

/** Determines how to extract data relevant for weighted, non-labeled
  * edges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `WkDiEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class WEdgeDescriptor[N, E[X] <: UnDiEdge[X] with WEdge[X], +C <: WEdgeCompanion[E]](
    edgeCompanion: C,
    customSerializer: Option[Serializer[_ <: WEdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[WEdgeParameters]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) = {
    val (n1, n2) = (edge._1, edge._2)
    new WEdgeParameters(
      descriptor.nodeDescriptor(edge._1) id (n1),
      descriptor.nodeDescriptor(edge._2) id (n2),
      edge.weight)
  }
}

/** Determines how to extract data relevant for non-weighted, labeled
  * edges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `LDiEdge`.
  * @param aLabel $LABEL
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class LEdgeDescriptor[N, E[X] <: UnDiEdge[X] with LEdge[X], +C <: LEdgeCompanion[E], L <: AnyRef](
    edgeCompanion: C,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: LEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]], implicit val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[LEdgeParameters[L]]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) = {
    val (n1, n2) = (edge._1, edge._2)
    new LEdgeParameters[edge.L1](
      descriptor.nodeDescriptor(n1) id (n1),
      descriptor.nodeDescriptor(n2) id (n2),
      edge.label).asInstanceOf[LEdgeParameters[L]]
  }
}

/** Determines how to extract data relevant for weighted, labeled
  * edges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `WLDiEdge`.
  * @param aLabel $LABEL
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class WLEdgeDescriptor[N, E[X] <: UnDiEdge[X] with WLEdge[X], +C <: WLEdgeCompanion[E], L <: AnyRef](
    edgeCompanion: C,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: WLEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]], implicit val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[WLEdgeParameters[L]]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]): WLEdgeParameters[L] = {
    val (n1, n2) = (edge._1, edge._2)
    new WLEdgeParameters[edge.L1](
      descriptor.nodeDescriptor(n1) id (n1),
      descriptor.nodeDescriptor(n2) id (n2),
      edge.weight,
      edge.label).asInstanceOf[WLEdgeParameters[L]]
  }
}

/** Determines how to extract data relevant for custom edges
  * from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion $COMPANION a custom edge companion.
  * @param attributes $ATTR
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class CEdgeDescriptor[N, E[X] <: CEdge[X], +C <: CEdgeCompanion[E], P <: Product with AnyRef](
    edgeCompanion: C,
    val sampleAttributes: P,
    customSerializer: Option[Serializer[_ <: CEdgeParameters[P]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]],
                                         implicit val attributeManifest: Manifest[P])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[CEdgeParameters[P]]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) = {
    val (n1, n2) = (edge._1, edge._2)
    new CEdgeParameters[edge.P](
      descriptor.nodeDescriptor(edge._1) id (n1),
      descriptor.nodeDescriptor(edge._2) id (n2),
      edge.attributes).asInstanceOf[CEdgeParameters[P]]
  }
}

/** Determines how to extract data relevant for non-weighted, non-labeled
  * hyperedges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `DiHyperEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class HyperEdgeDescriptor[N, E[X] <: HyperEdge[X], +C <: HyperEdgeCompanion[E]](
    edgeCompanion: C,
    customSerializer: Option[Serializer[_ <: HyperEdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[HyperEdgeParameters]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) =
    new HyperEdgeParameters(nodeIds(edge, descriptor), CollectionKind.from(edge).toString)
}

/** Determines how to extract data relevant for weighted, non-labeled
  * hyperedges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `WDiHyperEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class WHyperEdgeDescriptor[N, E[X] <: WHyperEdge[X], +C <: WHyperEdgeCompanion[E]](
    edgeCompanion: C,
    customSerializer: Option[Serializer[_ <: WHyperEdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[WHyperEdgeParameters]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) =
    new WHyperEdgeParameters(nodeIds(edge, descriptor), CollectionKind.from(edge).toString, edge.weight)
}

/** Determines how to extract data relevant for non-weighted, labeled
  * hyperedges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `LDiHyperEdge`.
  * @param aLabel $LABEL
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class LHyperEdgeDescriptor[N, E[X] <: LHyperEdge[X], +C <: LHyperEdgeCompanion[E], L <: AnyRef](
    edgeCompanion: C,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: LHyperEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]], implicit val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {
  override def extract(jsonEdge: JValue) = jsonEdge.extract[LHyperEdgeParameters[L]]
  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]): LHyperEdgeParameters[L] =
    new LHyperEdgeParameters(nodeIds(edge, descriptor), CollectionKind.from(edge).toString, edge.label)
      .asInstanceOf[LHyperEdgeParameters[L]]
}

/** Determines how to extract data relevant for weighted, labeled
  * hyperedges from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion  $COMPANION `WLDiHyperEdge`.
  * @param aLabel $LABEL
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class WLHyperEdgeDescriptor[N, E[X] <: WLHyperEdge[X], +C <: WLHyperEdgeCompanion[E], L <: AnyRef](
    edgeCompanion: C,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: WLHyperEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]], implicit val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {

  override def extract(jsonEdge: JValue) = jsonEdge.extract[WLHyperEdgeParameters[L]]

  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]): WLHyperEdgeParameters[L] =
    new WLHyperEdgeParameters(nodeIds(edge, descriptor), CollectionKind.from(edge).toString, edge.weight, edge.label)
      .asInstanceOf[WLHyperEdgeParameters[L]]
}

/** Determines how to extract data relevant for custom edges
  * from a JValue and how to decompose such an outer edge to a JValue.
  *
  * $USAGE
  * @param edgeCompanion $COMPANION a custom edge companion.
  * @param attributes $ATTR
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class CHyperEdgeDescriptor[N, E[X] <: CHyperEdge[X], +C <: CHyperEdgeCompanion[E], P <: Product with AnyRef](
    edgeCompanion: C,
    val sampleAttributes: P,
    customSerializer: Option[Serializer[_ <: CEdgeParameters[P]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId)(implicit edgeManifest: ClassTag[E[N]],
                                         implicit val attributeManifest: Manifest[P])
    extends EdgeDescriptorBase[N, E, C](edgeCompanion, customSerializer, extraClasses, typeId)(edgeManifest) {

  override def extract(jsonEdge: JValue) = jsonEdge.extract[CEdgeParameters[P]]

  override protected def toParameters(edge: E[N])(implicit descriptor: Descriptor[N]) =
    new CHyperEdgeParameters[edge.P](nodeIds(edge, descriptor), CollectionKind.from(edge).toString, edge.attributes)
      .asInstanceOf[CHyperEdgeParameters[P]]
}
