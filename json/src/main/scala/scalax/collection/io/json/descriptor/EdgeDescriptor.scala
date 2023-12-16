package scalax.collection.io.json
package descriptor

import scala.reflect.ClassTag

import net.liftweb.json._

import scalax.collection.{OneOrMore, Several}
import scalax.collection.generic._
import scalax.collection.io.edge.Types._
import scalax.collection.io.edge._

/** Generic base trait for any `*EdgeDescriptor` excluding edge types
  * to be used as type argument to collections containing edge descriptors of different types.
  */
sealed abstract class GenEdgeDescriptor[N](val classTag: ClassTag[_], override val typeId: String)
    extends TypeId(typeId)

/** Base trait for any `class *EdgeDescriptor`.
  *
  * @define USAGE Instances of this class must be passed as a constructor argument to a
  *         [[scalax.collection.io.json.descriptor.Descriptor]] either directly or
  *         indirectly by utilizing a predefined edge descriptor.
  * @define FACTORY factory method to create
  * @define CUSTSER `Some` list-json custom serializer or `None`.
  * @define EXTRACL list of classes referenced by this node provided they are to be (de)serialized; empty by default.
  * @define TYPEID denotes the edge type in a JSON text.
  * @define LABEL any sample value for the label type `L` such as `""` for `String`.
  * @define ATTR any sample value for the `Product` of custom attributes.
  *
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
sealed abstract class EdgeDescriptorBase[N, E <: Edge[N]](
    val customSerializer: Option[Serializer[_ <: Parameters]] = None,
    val extraClasses: List[Class[_]] = Nil,
    typeId: String
)(implicit classTag: ClassTag[E])
    extends GenEdgeDescriptor[N](classTag, typeId) {

  implicit protected val formats: Formats = Serialization.formats(
    if (extraClasses.isEmpty) NoTypeHints
    else ShortTypeHints(extraClasses)
  ) ++ customSerializer

  def extract(jsonEdge: JValue): Parameters

  final def decompose(edge: E)(implicit descriptor: Descriptor[N]): JValue =
    Extraction.decompose(toParameters(edge))

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]): Parameters

  protected def nodeIds(edge: E, descriptor: Descriptor[N]): SeveralNodeIds =
    edge.ends.map(n => descriptor.nodeDescriptor(n) id n)

  protected def nodeIds(ends: OneOrMore[N], descriptor: Descriptor[N]): OneOrMoreNodeIds =
    ends.map(n => descriptor.nodeDescriptor(n) id n)
}

protected trait Label[E <: Edge[_]] {

  /** Extracts the label part of the typed edge into any convenient type. */
  protected def label(edge: E): Any
}

/** Determines how to extract data of non-labeled edges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param edgeFactory  $FACTORY `DiEdge` or `UnDiEdge`.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class EdgeDescriptor[N, E <: AnyEdge[N]](
    val edgeFactory: (N, N) => E,
    customSerializer: Option[Serializer[_ <: EdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit classTag: ClassTag[E])
    extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag) {

  def extract(jsonEdge: JValue): EdgeParameters = jsonEdge.extract[EdgeParameters]

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]) = {
    val (n1, n2) = (edge.node1, edge.node2)
    EdgeParameters(descriptor.nodeDescriptor(n1) id n1, descriptor.nodeDescriptor(n2) id n2)
  }
}

/** Determines how to extract data of labeled edges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param edgeFactory  $FACTORY `WUnDiEdge`, `WDiEdge` or any other user-defined typed edge.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
abstract class LEdgeDescriptor[N, E <: AnyEdge[N], L](
    val edgeFactory: (N, N, L) => E,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: LEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit
    classTag: ClassTag[E],
    val labelManifest: Manifest[L]
) extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag)
    with Label[E] {

  def extract(jsonEdge: JValue): LEdgeParameters[L] = jsonEdge.extract[LEdgeParameters[L]]

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]) =
    LEdgeParameters(
      descriptor.nodeDescriptor(edge.node1) id edge.node1,
      descriptor.nodeDescriptor(edge.node2) id edge.node2,
      label(edge)
    )
}

/** Determines how to extract data of non-labeled hyperedges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param hyperEdgeFactory  $FACTORY any of the predefined `HyperEdge`s.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class HyperEdgeDescriptor[N, E <: AbstractHyperEdge[N]](
    val hyperEdgeFactory: Several[N] => E,
    customSerializer: Option[Serializer[_ <: HyperEdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit classTag: ClassTag[E])
    extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag) {

  def extract(jsonEdge: JValue): HyperEdgeParameters = jsonEdge.extract[HyperEdgeParameters]

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]) =
    HyperEdgeParameters(nodeIds(edge, descriptor))
}

/** Determines how to extract data of labeled hyperedges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param hyperEdgeFactory  $FACTORY any user-defined typed edge.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
abstract class LHyperEdgeDescriptor[N, E <: AbstractHyperEdge[N], L](
    val hyperEdgeFactory: (Several[N], L) => E,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: LHyperEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit classTag: ClassTag[E], val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag)
    with Label[E] {

  def extract(jsonEdge: JValue): LHyperEdgeParameters[L] = jsonEdge.extract[LHyperEdgeParameters[L]]

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]) =
    LHyperEdgeParameters(nodeIds(edge, descriptor), label(edge))
}

/** Determines how to extract data of non-labeled directed hyperedges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param diHyperEdgeFactory  $FACTORY any of the predefined `DiHyperEdge`s.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
class DiHyperEdgeDescriptor[N, E <: AbstractDiHyperEdge[N]](
    val diHyperEdgeFactory: (OneOrMore[N], OneOrMore[N]) => E,
    customSerializer: Option[Serializer[_ <: HyperEdgeParameters]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit classTag: ClassTag[E])
    extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag) {

  def extract(jsonEdge: JValue): DiHyperEdgeParameters = jsonEdge.extract[DiHyperEdgeParameters]

  protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]): DiHyperEdgeParameters =
    DiHyperEdgeParameters(nodeIds(edge.sources, descriptor), nodeIds(edge.targets, descriptor))
}

/** Determines how to extract data of labeled directed hyperedges from a JValue and how to decompose such an edge to a JValue.
  *
  * $USAGE
  * @param diHyperEdgeFactory $FACTORY any user-defined typed directed hyperedge.                                                                       diHyperEdgeFactory  $FACTORY any user-defined typed directed hyperedge.
  * @param customSerializer $CUSTSER
  * @param extraClasses $EXTRACL
  * @param typeId $TYPEID
  */
abstract class LDiHyperEdgeDescriptor[N, E <: AbstractDiHyperEdge[N], L](
    val diHyperEdgeFactory: (OneOrMore[N], OneOrMore[N], L) => E,
    val aLabel: L,
    customSerializer: Option[Serializer[_ <: LDiHyperEdgeParameters[L]]] = None,
    extraClasses: List[Class[_]] = Nil,
    typeId: String = Defaults.defaultId
)(implicit classTag: ClassTag[E], val labelManifest: Manifest[L])
    extends EdgeDescriptorBase[N, E](customSerializer, extraClasses, typeId)(classTag)
    with Label[E] {

  override def extract(jsonEdge: JValue): LDiHyperEdgeParameters[L] = jsonEdge.extract[LDiHyperEdgeParameters[L]]

  override protected def toParameters(edge: E)(implicit descriptor: Descriptor[N]): LDiHyperEdgeParameters[L] =
    LDiHyperEdgeParameters(
      nodeIds(edge.sources, descriptor),
      nodeIds(edge.targets, descriptor),
      label(edge).asInstanceOf[L]
    )
}
