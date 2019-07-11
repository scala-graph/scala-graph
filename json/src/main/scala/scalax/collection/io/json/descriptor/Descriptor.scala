package scalax.collection.io.json
package descriptor

import language.existentials
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
object DefaultSectionId extends SectionId("nodes", "edges")

object Defaults {
  val defaultId = "(default)"
}
import Defaults._

abstract class TypeId(val typeId: String)

/** Top level descriptor to be passed to Graph/JSON conversion methods, in particular to
  * `fromJson` and `toJson`.
  *
  * @param defaultNodeDescriptor the only or default node descriptor accepting/producing a
  *        flat node list, that is a node list without a node typeId.
  * @param defaultEdgeDescriptor the only or default edge descriptor accepting/producing a
  *        flat edge list, that is an edge list without an edge typeId.
  * @param namedNodeDescriptors further optional node descriptors accepting/producing named
  *        node lists, that is node lists with an explicit node typeId.
  * @param namedEdgeDescriptors further optional edge descriptors accepting/producing named
  *        edge lists, that is edge lists with an explicit edge typeId.
  * @param sectionIds denotes node/edge sections in a JSON text defaulting to `"nodes"`
  *        and `"edges"`.
  */
class Descriptor[N](val defaultNodeDescriptor: NodeDescriptor[N],
                    val defaultEdgeDescriptor: GenEdgeDescriptor[N],
                    namedNodeDescriptors: Iterable[NodeDescriptor[N]] = Seq.empty[NodeDescriptor[N]],
                    namedEdgeDescriptors: Iterable[GenEdgeDescriptor[N]] = Seq.empty[GenEdgeDescriptor[N]],
                    val sectionIds: SectionId = DefaultSectionId) {
  def requireUniqueTypeIds(descriptors: Iterable[TypeId]) {
    def duplicateTypeId =
      (namedNodeDescriptors.map(_.typeId).toList).sorted sliding 2 find
        (strings => if (strings.size == 2) strings.head == strings.tail else false)
    val duplNodeTypeId = duplicateTypeId
    require(duplNodeTypeId.isEmpty, "Duplicate typeId found: " + duplNodeTypeId.get.head)
  }
  requireUniqueTypeIds(namedNodeDescriptors)
  requireUniqueTypeIds(namedEdgeDescriptors)

  protected val nodeDescriptors = Seq(defaultNodeDescriptor) ++ namedNodeDescriptors
  protected val edgeDescriptors = Seq(defaultEdgeDescriptor) ++ namedEdgeDescriptors

  def nodeDescriptor(typeId: String): Option[NodeDescriptor[N]] =
    if (typeId == defaultId ||
        typeId == defaultNodeDescriptor.typeId)
      Some(defaultNodeDescriptor)
    else
      namedNodeDescriptors find (_.typeId == typeId)

  def edgeDescriptor(typeId: String): Option[GenEdgeDescriptor[N]] =
    if (typeId == defaultId ||
        typeId == defaultEdgeDescriptor.typeId)
      Some(defaultEdgeDescriptor)
    else
      namedEdgeDescriptors find (_.typeId == typeId)

  protected lazy val nodeDescriptorsByManifest: Map[ClassTag[_], NodeDescriptor[N]] = {
    val ret = collection.mutable.Map.empty[ClassTag[_], NodeDescriptor[N]]
    for {
      descr <- nodeDescriptors
      manifests = descr.manifests
      m <- manifests
    } ret += (m -> descr)
    ret.toMap
  }

  final protected def classManifest(any: Any): ClassTag[_] =
    ClassTag(any match {
      case r: AnyRef => r.getClass
      case v         => v.asInstanceOf[AnyRef].getClass
    })

  protected var lastNodeDescriptor: (Class[_], NodeDescriptor[N]) = (classOf[Null], null)

  def nodeDescriptor(node: N): NodeDescriptor[N] = {
    val clazz = node match {
      case r: AnyRef => Some(r.getClass)
      case _         => None
    }
    if (clazz.filter(_ == lastNodeDescriptor._1).isDefined)
      lastNodeDescriptor._2
    else {
      val descr =
        nodeDescriptorsByManifest
          .find(_._1.runtimeClass == classManifest(node).runtimeClass)
          .flatMap(kv => Some(kv._2)) getOrElse ((nodeDescriptorsByManifest find (classTag =>
          manifest.runtimeClass.isAssignableFrom(classTag._1.runtimeClass)))
          .flatMap(kv => Some(kv._2)) getOrElse (throw err(
          NoNodeDescr,
          clazz flatMap (c => Some(c.getName)) getOrElse ("AnyVal"))))
      if (clazz.isDefined)
        lastNodeDescriptor = (clazz.get, descr)
      descr
    }
  }

  def edgeDescriptor(clazz: Class[_]): GenEdgeDescriptor[N] = {
    val className       = clazz.getName
    val classNameLength = className.length
    edgeDescriptors find { d =>
      val dClassName       = d.edgeManifest.runtimeClass.getName
      val dClassNameLength = dClassName.length
      dClassName == (if (dClassNameLength < classNameLength)
                       className substring (0, dClassNameLength)
                     else
                       className)
    } getOrElse (throw err(NoEdgeDescr, clazz.getName))
  }
}
