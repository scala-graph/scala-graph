package scalax.collection.io.json
package descriptor

import reflect.ClassTag

import error.JsonGraphError._

/** Contains string constants to denote node/edge sections in a JSON text.
  *
  * An individual instance of this class may be passed to `Descriptor` if
  * non-default section id's are to be used.
  */
class SectionId(val nodesId: String, val edgesId: String) {

  /** Returns whether `id` is one of `nodesId` or `edgesId` of this `SectionId`. */
  def contains(id: String) = isNodes(id) || isEdges(id)

  /** Returns whether `id` equals to `nodesId` of this `SectionId`. */
  def isNodes(id: String) = id == nodesId

  /** Returns whether `id` equals to `edgesId` of this `SectionId`. */
  def isEdges(id: String) = id == edgesId
}

/** The default section id's `"nodes"` and `"edges"`. */
object DefaultSectionIds extends SectionId("nodes", "edges")

object Defaults {
  val defaultId = "(default)"
}
import Defaults._

abstract class TypeId(val typeId: String)

/** Top level descriptor to be passed to Graph/JSON conversion methods, in particular to
  * `fromJson` and `toJson`.
  *
  * @param defaultNodeDescriptor the only or default node descriptor accepting/producing a
  *        flat node list, that is a node list without `typeId`s.
  * @param defaultEdgeDescriptor the only or default edge descriptor accepting/producing a
  *        flat edge list, that is an edge list without an edge typeId.
  * @param namedNodeDescriptors further optional node descriptors accepting/producing named
  *        node lists, that is node lists with an explicit node typeId.
  * @param namedEdgeDescriptors further optional edge descriptors accepting/producing named
  *        edge lists, that is edge lists with an explicit edge typeId.
  * @param sectionIds denotes node/edge sections in a JSON text defaulting to `"nodes"`
  *        and `"edges"`.
  */
final class Descriptor[N](
    nodeDescriptors: String Map NodeDescriptor[N],
    edgeDescriptors: String Map GenEdgeDescriptor[N],
    val sectionIds: SectionId = DefaultSectionIds
) {
  require(nodeDescriptors.nonEmpty, "At least one NodeDescriptor expected.")
  require(edgeDescriptors.nonEmpty, "At least one EdgeDescriptor expected.")

  val hasSingleNodeDescriptor: Boolean = nodeDescriptors.size == 1
  val hasSingleEdgeDescriptor: Boolean = edgeDescriptors.size == 1

  private def headOrGet[A](typeId: String, map: String Map A): Option[A] =
    if (typeId == defaultId) map.values.headOption
    else map get typeId

  def nodeDescriptor(typeId: String): Option[NodeDescriptor[N]]    = headOrGet(typeId, nodeDescriptors)
  def edgeDescriptor(typeId: String): Option[GenEdgeDescriptor[N]] = headOrGet(typeId, edgeDescriptors)

  private lazy val nodeDescriptorsByManifest: Map[ClassTag[_], NodeDescriptor[N]] = {
    val ret = collection.mutable.Map.empty[ClassTag[_], NodeDescriptor[N]]
    for {
      descr <- nodeDescriptors.valuesIterator
      manifests = descr.manifests
      m <- manifests
    } ret += m -> descr
    ret.toMap
  }

  private def classManifest(any: Any): ClassTag[_] =
    ClassTag(any match {
      case r: AnyRef => r.getClass
      case v         => v.asInstanceOf[AnyRef].getClass
    })

  private var lastNodeDescriptor: (Class[_], NodeDescriptor[N]) = (classOf[Null], null)

  def nodeDescriptor(node: N): NodeDescriptor[N] = {
    val clazz: Option[Class[_]] = node match {
      case r: AnyRef => Some(r.getClass)
      case _         => None
    }
    if (clazz.contains(lastNodeDescriptor._1)) lastNodeDescriptor._2
    else {
      val descr =
        nodeDescriptorsByManifest
          .find(_._1.runtimeClass == classManifest(node).runtimeClass)
          .flatMap(kv => Some(kv._2)) getOrElse ((nodeDescriptorsByManifest find (classTag =>
          manifest.runtimeClass.isAssignableFrom(classTag._1.runtimeClass)
        )).flatMap(kv => Some(kv._2)) getOrElse (throw err(
          NoNodeDescr,
          clazz flatMap (c => Some(c.getName)) getOrElse "AnyVal"
        )))
      if (clazz.isDefined)
        lastNodeDescriptor = (clazz.get, descr)
      descr
    }
  }

  def edgeDescriptor(clazz: Class[_]): GenEdgeDescriptor[N] = {
    val className       = clazz.getName
    val classNameLength = className.length
    edgeDescriptors.valuesIterator find { d =>
      val dClassName       = d.classTag.runtimeClass.getName
      val dClassNameLength = dClassName.length
      dClassName == (if (dClassNameLength < classNameLength)
                       className substring (0, dClassNameLength)
                     else
                       className)
    } getOrElse (throw err(NoEdgeDescr, clazz.getName))
  }
}

object Descriptor {
  def simple[N](
      nodeDescriptor: NodeDescriptor[N],
      edgeDescriptor: GenEdgeDescriptor[N],
      sectionIds: SectionId = DefaultSectionIds
  ) = new Descriptor[N](
    Map(nodeDescriptor.typeId -> nodeDescriptor),
    Map(edgeDescriptor.typeId -> edgeDescriptor),
    sectionIds
  )

  def apply[N](
      nodeDescriptors: NodeDescriptor[N]*
  )(edgeDescriptors: GenEdgeDescriptor[N]*)(sectionIds: SectionId = DefaultSectionIds) =
    new Descriptor[N](
      nodeDescriptors.iterator.map(d => d.typeId -> d).toMap,
      edgeDescriptors.iterator.map(d => d.typeId -> d).toMap,
      sectionIds
    )
}
