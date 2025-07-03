package scalax.collection.io.json
package descriptor

import reflect.ClassTag

import error.JsonGraphError._

/** Contains string constants that are used as JSON keys to denote the node and edge sections in the JSON text.
  *
  * An individual instance of this class may be passed to `Descriptor` if non-default section id's are to be used.
  */
case class SectionKeys(nodesId: String = "nodes", edgesId: String = "edges") {

  /** Returns whether `id` is one of `nodesId` or `edgesId` of this `SectionId`. */
  def contains(id: String): Boolean = isNodes(id) || isEdges(id)

  /** Returns whether `id` equals to `nodesId` of this `SectionId`. */
  def isNodes(id: String): Boolean = id == nodesId

  /** Returns whether `id` equals to `edgesId` of this `SectionId`. */
  def isEdges(id: String): Boolean = id == edgesId
}

object SectionKeys {

  /** The default section keys, namely `"nodes"` and `"edges"`. */
  val default = SectionKeys()
}

object Defaults {
  val defaultId = "(default)"
}
import Defaults._

abstract class TypeId(val typeId: String)

/** Top level descriptor to be passed to Graph/JSON conversion methods, in particular to `fromJson` and `toJson`.
  *
  * @param nodeDescriptors non-empty `Map` of type Ids to `NodeDscriptor`s.
  * @param edgeDescriptors non-empty `Map` of type Ids to `EdgeDscriptor`s.
  * @param sectionKeys the JSON keys to denote node and edge sections in the JSON text.
  * @tparam N the type of graph nodes.
  * @throws `IllegalArgumentException` if any of the descriptor maps is empty.
  */
final class Descriptor[N](
    nodeDescriptors: String Map NodeDescriptor[N],
    edgeDescriptors: String Map GenEdgeDescriptor[N],
    val sectionKeys: SectionKeys = SectionKeys.default
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

  /** Creates a `Descriptor` with a single `NodeDescriptor` and a single `EdgeDescriptor`.
    *
    * @param sectionKeys the JSON keys to denote node and edge sections in the JSON text.
    * @tparam N the type of nodes
    */
  def simple[N](
      nodeDescriptor: NodeDescriptor[N],
      edgeDescriptor: GenEdgeDescriptor[N],
      sectionKeys: SectionKeys = SectionKeys.default
  ) = new Descriptor[N](
    Map(nodeDescriptor.typeId -> nodeDescriptor),
    Map(edgeDescriptor.typeId -> edgeDescriptor),
    sectionKeys
  )

  /** Creates a `Descriptor` with any number of `NodeDescriptor`s and `EdgeDescriptor`s with distinct type Ids.
    *
    * @param sectionKeys the JSON keys to denote node and edge sections in the JSON text.
    * @tparam N the type of graph nodes.
    * @throws `IllegalArgumentException` if any of the descriptor parameter lists is empty.
    */
  def apply[N](
      nodeDescriptors: NodeDescriptor[N]*
  )(edgeDescriptors: GenEdgeDescriptor[N]*)(sectionKeys: SectionKeys = SectionKeys.default) =
    new Descriptor[N](
      nodeDescriptors.iterator.map(d => d.typeId -> d).toMap,
      edgeDescriptors.iterator.map(d => d.typeId -> d).toMap,
      sectionKeys
    )
}
