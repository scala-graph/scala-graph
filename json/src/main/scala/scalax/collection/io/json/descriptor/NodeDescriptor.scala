package scalax.collection.io.json
package descriptor

import net.liftweb.json._

import error.JsonGraphError._

/** Provides information on how to extract node data from a JValue and how to
  * decompose the node to a JValue.
  *
  * @tparam N type of nodes described with this descriptor which either the same type or
  *           a subtype of the node type parameter of the targeted graph.
  * @param typeId denotes the node type in a JSON text; defaults to `(default)`.
  * @param customSerializers list of list-json custom serializers to be invoked for node
  *        serialization, defaulting to an empty list.
  * @param extraClasses list of classes referenced by this node provided they are to be
  *        (de)serialized; empty by default.
  * @param furtherManifests list of manifests denoting subtypes of `N` which are to be
  *        processed, defaulting to an empty list.
  */
abstract class NodeDescriptor[+N](override val typeId: String = Defaults.defaultId,
                                  customSerializers: Traversable[Serializer[_]] = Nil,
                                  extraClasses: List[Class[_]] = Nil,
                                  furtherManifests: List[Manifest[_]] = Nil)(implicit nodeManifest: Manifest[N])
    extends TypeId(typeId) {
  val manifests = nodeManifest :: furtherManifests
  implicit val formats = Serialization.formats(
    if (extraClasses.isEmpty) NoTypeHints
    else new ShortTypeHints(extraClasses)) ++ customSerializers
  def extract(jsonNode: JValue): N = jsonNode.extract[N]
  def decompose(node: Any): JValue = Extraction.decompose(node)

  /** Enables Graph for Scala JSON export/import to handle node references in JSON edge entries.
    * Without establishing such references, JSON edge entries would have to contain all node
    * data what would make JSON texts representing graphs explode in length.
    *
    * Please exercise great care when designing the id method to return unique keys.
    *
    * @param node a node of type `N` for which its unique id is to be returned.
    *             You can safely match `node` to the actual type argument `N`.
    */
  def id(node: Any): String
}

/** Node descriptor extracting a String from any JValue and decomposing nodes
  * of any type to a JString. This object serves mainly test purposes.
  */
object StringNodeDescriptor extends NodeDescriptor[String] {
  override def extract(jsonNode: JValue) = {
    def mkString(jValues: Traversable[JValue]): String =
      (for (fld <- jValues) yield {
        fld match {
          case JString(s)           => s
          case JInt(_) | JDouble(_) => fld.extract[String]
          case JBool(b)             => b.toString
          case JArray(_)            => "(" + mkString(fld.children) + ")"
          case JObject(obj) =>
            val buf = new StringBuilder("(")
            obj.foldLeft(buf)((buf, o) => buf.append(o.name + "," + mkString(List(o.value))))
            buf.append(")").toString
          case JNull    => "Null"
          case JNothing => "Nothing"
        }
      }) mkString ","
    if (jsonNode.children.nonEmpty) mkString(jsonNode.children)
    else throw err(EmptyNodeFieldList)
  }
  override def decompose(node: Any): JValue = JArray(List(super.decompose(node.toString)))
  override def id(node: Any)                = node.toString
}
